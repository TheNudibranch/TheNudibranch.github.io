//// Define Variables
var btn_state = false;
var zPts = []; 
var xPts = [];
var yPts = [];
var t = 0;
var t_step = 0.06;
var xy_step = 0.2;
var x_min = -5;
var x_max = 5;
var y_min = -5;
var y_max = 5;

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

//// Create XY Grid
for(x=x_min; x<x_max; x+=xy_step) {
  let zTemp = [];
  let yTemp = [];
  let xTemp = [];
  for (y=y_min; y<y_max; y+=xy_step) {
    zTemp.push(Math.cos(Math.sqrt(x**2 + y**2) + t));
    yTemp.push(y);
    xTemp.push(x);
  }
  zPts.push(zTemp);
  yPts.push(yTemp);
  xPts.push(xTemp);
  }

var xyz = math.matrix([xPts, yPts, zPts]);
var m_range = math.range(0,xPts.length);
var full_data_xy = get_sub_matrix(xyz,)


//// Matrix Helper Functions
function get_sub_matrix(mat, index){
    return(math.squeeze(math.subset(mat, math.index(index, m_range, m_range))));
}
function wide_to_long(mat){
    // Must take a square 2D mat
    return(math.flatten(mat));
}

function long_to_wide(mat){
    // Must take a 1D array
    return(math.reshape(mat, [m_range._size[0], m_range._size[0]]));
}

var data = [{
    z: math.squeeze(math.subset(xyz, math.index(2, m_range, m_range)))._data,
    x: xPts,
    y: yPts,
    showscale : false,
    type: 'surface'},
    {
    z: [1,1],
    y: [0,0],
    x: [1,0],
    mode: 'markers',
    type: 'scatter3d'
    }
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

function compute(){
    zPts = math.add(get_sub_matrix(xyz,0).map(math.square), get_sub_matrix(xyz,1).map(math.square)).map(x => math.cos(math.sqrt(x) + t))._data
    data = [{
    z: zPts,
    x: xPts,
    y: yPts,
    type: 'surface'
    }];
}

function update () {
    if (btn_state) {
        compute();
        t += t_step;
        Plotly.animate('myDiv', {data: [{z: zPts, x: xPts, y: yPts,type: 'surface'}]} , {
            transition: {
                duration: 0
            },
            frame: {
                duration: 0,
                redraw: true
            }
            }
        );

        requestAnimationFrame(update);
    }

}
  
Plotly.newPlot('myDiv', data, layout, {displayModeBar: false});

requestAnimationFrame(update);
