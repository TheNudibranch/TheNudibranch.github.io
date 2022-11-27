var btn_state = false;
var grid_min = -5;
var grid_max = 5;
var mat_length = 15;
var resolution = (grid_max - grid_min) / mat_length;
var leap_steps = 10;
var step_size = 0.001;

var x1 = []; var x2 = [];

i_val = grid_min - resolution;
j_val = grid_min - resolution;
for (i =0; i < mat_length; i++){
    i_val += resolution;
    j_val = grid_min - resolution;
    for (j = 0; j < mat_length; j++){
        j_val += resolution;
        x1.push(i_val);
        x2.push(j_val);
    }
}
x1_mat = math.reshape(x1, [mat_length, mat_length]);
x2_mat = math.reshape(x2, [mat_length, mat_length]);

//// Function to parse observation data
function parse_text_area(silent=false){
    var t_area = document.getElementById("post_data");
    var lines = t_area.value.replace(/\r\n/g,"\n").split("\n"); 
    var post_data = [];

    for (i=0;i<lines.length;i++){
        var dat = lines[i].slice(lines[i].indexOf('(')+1, lines[i].indexOf(')')).split(',').map(x => parseInt(x));
        post_data.push(dat);
    }
    post_data_mat = math.matrix(post_data);
    
    return(math.matrix(post_data));
}

observations = parse_text_area();


var full_grid_xy = math.concat(
    math.concat(math.reshape(x1, [-1,1]),math.reshape(x2, [-1,1])), 
    math.subset(observations, math.index(math.range(0,observations._size[0])._data, [0,1])),
    0)._data;

//// Set up functions
function calc_kernel(full_grid, l_param, a_param, noise){
    var k_mat = [];
    for(i=0; i<full_grid.length; i+=1){
        for (j=0; j<full_grid.length; j+=1){
            var a = a_param * math.exp((((full_grid[i][0] - full_grid[j][0]) / l_param)**2 + ((full_grid[i][1] - full_grid[j][1]) / l_param)**2) * (-0.5));
            k_mat.push(a);
        }
    }
    var mat_inter = math.reshape(k_mat, [full_grid.length, full_grid.length]);

    return math.add(mat_inter, math.multiply(noise, math.identity(full_grid.length)))._data;
}

function marginal_mean(kernal_mat, obs){
    var mat_1 = math.subset(kernal_mat, 
                        math.index(
                            math.range(0,kernal_mat.length - obs._size[0]), 
                            math.range(kernal_mat.length - obs._size[0], kernal_mat.length)
                        )); // (long grid length, obs length)
    var mat_2 = math.inv(math.subset(kernal_mat,
                        math.index(
                            math.range(kernal_mat.length - obs._size[0], kernal_mat.length),
                            math.range(kernal_mat.length - obs._size[0], kernal_mat.length)
                        ))); // (obs length, obs length)
    var mat_3 = math.squeeze(math.multiply(math.multiply(mat_1, mat_2), math.subset(obs, math.index(math.range(0,obs._size[0]), [2]))));
    return(mat_3);
}

function marginal_cov(kernal_mat, obs){
    var mat_1 = math.subset(kernal_mat, 
                        math.index(
                            math.range(0,kernal_mat.length - obs._size[0]), 
                            math.range(0,kernal_mat.length - obs._size[0])
                        )); // (long grid length, long grid length)
    var mat_2 = math.subset(kernal_mat, 
                    math.index(
                        math.range(0,kernal_mat.length - obs._size[0]), 
                        math.range(kernal_mat.length - obs._size[0], kernal_mat.length)
                    )); // (long grid length, obs length)
    var mat_3 = math.inv(math.subset(kernal_mat,
                            math.index(
                                math.range(kernal_mat.length - obs._size[0], kernal_mat.length),
                                math.range(kernal_mat.length - obs._size[0], kernal_mat.length)
                            ))); // (obs length, obs length)
    var mat_4 = math.subset(kernal_mat, 
                    math.index(
                        math.range(kernal_mat.length - obs._size[0], kernal_mat.length), 
                        math.range(0,kernal_mat.length - obs._size[0])
                    )); // (obs length,long grid length)
    
    var mat_5 = math.subtract(mat_1, math.multiply(math.multiply(mat_2, mat_3), mat_4));
    return(mat_5);
}


k_mat = calc_kernel(full_grid_xy, 1, 1, 0.1);
mu = marginal_mean(k_mat, observations);
cov = marginal_cov(k_mat, observations);
cov_inv = math.inv(cov);
curr_y = mu._data;
mom = window.MultivariateNormal.default(math.resize(0,[mu._size[0]])._data, math.identity(mu._size[0])._data);
// gp_dist = window.MultivariateNormal.default(mu._data, math.round(cov, 4));
// curr_y = gp_dist.sample();


//// HMC functions
function gradient(y){
    return math.multiply(cov_inv, math.subtract(y, mu))
}

function normlog_gp(y){
    return math.multiply(math.multiply(math.transpose(math.subtract(y, mu)), cov_inv), math.subtract(y, mu)) * -0.5
}

function normlog_mom(m){
    return math.dot(math.subtract(m, mom.getMean()), math.subtract(m, mom.getMean())) * -0.5
}

function copyToClipboard(text) {
    var dummy = document.createElement("textarea");
    // to avoid breaking orgain page when copying more words
    // cant copy when adding below this code
    // dummy.style.display = 'none'
    document.body.appendChild(dummy);
    //Be careful if you use texarea. setAttribute('value', value), which works with "input" does not work with "textarea". – Eduard
    dummy.value = text;
    dummy.select();
    document.execCommand("copy");
    document.body.removeChild(dummy);
}

function hmc_step(){
    var starting_mom = mom.sample();
    var proposed = leapfrog(starting_mom);
    var loglike_curr_y = normlog_gp(curr_y);
    var loglike_prop_y = normlog_gp(proposed.y);
    var loglike_curr_m = normlog_mom(starting_mom);
    var loglike_prop_m = normlog_mom(proposed.m);
    var alpha = (loglike_prop_y + loglike_prop_m) - (loglike_curr_y + loglike_curr_m);
    // return({c_y: curr_y, p_y: proposed.y, c_m: starting_mom, p_m:proposed.m, alpha:alpha})
    if (math.exp(alpha) > math.random()){
        curr_y = proposed.y._data;
        console.log(1);
    }   
}
// mom_half <- momentum + (step_size / 2) * gradient(curr_x)
// curr_x <- curr_x + step_size*(m_mat %*% mom_half)
// momentum <- mom_half + (step_size / 2) * gradient(curr_x)
// curr_step <- curr_step + step_size
function leapfrog(m){
    var curr_y_leap = curr_y;
    var curr_m_leap = m;
    var mom_half = m;
    for(l=0; l<leap_steps; l++){
        mom_half = math.add(curr_m_leap, math.multiply(step_size/2, gradient(curr_y_leap)));
        curr_y_leap = math.add(curr_y_leap, math.multiply(step_size, mom_half));
        curr_m_leap = math.add(mom_half, math.multiply(step_size/2, gradient(curr_y_leap)));
    }
    return({y:curr_y_leap, m:curr_m_leap})
}

function compute(){
    hmc_step();
    curr_y_mat = math.reshape(curr_y, [mat_length, mat_length]);
}

function update(){
    if (btn_state){
        compute();
        Plotly.animate('myDiv', {data: [{z: curr_y_mat, x: x1_mat, y: x2_mat,type: 'surface'}]} , {
            transition: {
                duration: 0
            },
            frame: {
                duration: 0,
                redraw: true
            }})
        requestAnimationFrame(update);
    }
}

var curr_y_mat = math.reshape(curr_y, [mat_length, mat_length]);

var data = [{
    z: curr_y_mat,
    x: x1_mat,
    y: x2_mat,
    showscale : false,
    type: 'surface'}
];
  
var layout = {
title: 'My Plot',
autosize: true,  
width: 900,
height: 700,
margin: {
    l: 65,
    r: 50,
    b: 65,
    t: 90,}
};

Plotly.newPlot('myDiv', data, layout, {displayModeBar: false});

requestAnimationFrame(update);