// set the dimensions and margins of the graph
var margin = { top: 30, right: 30, bottom: 30, left: 50 },
  width = 460 - margin.left - margin.right,
  height = 400 - margin.top - margin.bottom;

//////////////////// PRIOR DISTRIBUTION
// append the svg object to the body of the page
var prior_svg = d3.select("#prior_viz")
  .append("svg")
  .attr("width", width + margin.left + margin.right)
  .attr("height", height + margin.top + margin.bottom)
  .append("g")
  .attr("transform",
    "translate(" + margin.left + "," + margin.top + ")");

// add the x Axis
var x = d3.scaleLinear()
  .domain([0, 1])
  .range([0, width]);
var x_axis = prior_svg.append("g")
  .attr('st', 'axisRed')
  .attr("transform", "translate(0," + height + ")")
  .call(d3.axisBottom(x));

x_axis.selectAll("line").style("stroke", "white");
x_axis.selectAll('text').style('stroke', 'white');
x_axis.selectAll('path').style('stroke', 'white');

var beta_dist = gen_beta_dist(parseFloat(document.getElementById('alpha_num').value), 
  parseFloat(document.getElementById('beta_num').value));

// add the y Axis
var y = d3.scaleLinear()
  .range([height, 0])
  .domain([0, jStat.max(beta_dist.map(function(x) {return x[1]})) * 1.2]);
var prior_y_axis = prior_svg.append("g")
  .call(d3.axisLeft(y));

prior_y_axis.selectAll("line").style("stroke", "white");
prior_y_axis.selectAll('text').style('stroke', 'white');
prior_y_axis.selectAll('path').style('stroke', 'white');


// Plot the area
var prior = prior_svg.append("path")
  // .attr("class", "mypath")
  .datum(beta_dist)
  .attr("fill", "#69b3a2")
  .attr("opacity", ".8")
  .attr("stroke", "#000")
  .attr("stroke-width", 1)
  .attr("stroke-linejoin", "round")
  .attr("d", d3.area()
    .curve(d3.curveBasis)
    .x(function (d) { return x(d[0]); })
    .y1(function (d) { return y(d[1]); })
    .y0(y(0))
  );


//////////////////// PRIOR PREDICTIVE DISTRIBUTION
var prior_predictive_svg = d3.select("#prior_predictive_viz")
  .append("svg")
  .attr("width", width + margin.left + margin.right)
  .attr("height", height + margin.top + margin.bottom)
  .append("g")
  .attr("transform",
    "translate(" + margin.left + "," + margin.top + ")");

var prior_predictive_dist = gen_beta_binomial_dist(parseFloat(document.getElementById('n_num').value), 
  parseFloat(document.getElementById('alpha_num').value), 
  parseFloat(document.getElementById('beta_num').value));

var x = d3.scaleBand()
  .range([ 0, width ])
  .domain(prior_predictive_dist.map(function(x) {return x[0];}))
  .padding(0.2);

prior_predictive_x_axis = prior_predictive_svg.append("g")
  .attr("transform", "translate(0," + height + ")")
  .call(d3.axisBottom(x));

prior_predictive_x_axis.selectAll("line").style("stroke", "white");
prior_predictive_x_axis.selectAll('text').style('stroke', 'white');
prior_predictive_x_axis.selectAll('path').style('stroke', 'white');

var y = d3.scaleLinear()
  .domain([0, jStat.max(prior_predictive_dist.map(function(x) {return x[1];}))*1.2])
  .range([ height, 0]);
prior_predictive_y_axis = prior_predictive_svg.append("g")
  .call(d3.axisLeft(y));

prior_predictive_y_axis.selectAll("line").style("stroke", "white");
prior_predictive_y_axis.selectAll('text').style('stroke', 'white');
prior_predictive_y_axis.selectAll('path').style('stroke', 'white');

var u = prior_predictive_svg.selectAll("rect")
  .data(prior_predictive_dist);

u.enter()
.append("rect")
.merge(u)
  .attr("x", function(d) { return x(d[0]); })
  .attr("y", function(d) { return y(d[1]); })
  .attr("width", x.bandwidth())
  .attr("height", function(d) { return height - y(d[1]); })
  .attr("fill", "#69b3a2")


//////////////////// POSTERIOR DISTRIBUTION
// append the svg object to the body of the page
var posterior_svg = d3.select("#posterior_viz")
  .append("svg")
  .attr("width", width + margin.left + margin.right)
  .attr("height", height + margin.top + margin.bottom)
  .append("g")
  .attr("transform",
    "translate(" + margin.left + "," + margin.top + ")");

// add the x Axis
var x = d3.scaleLinear()
  .domain([0, 1])
  .range([0, width]);
var x_axis = posterior_svg.append("g")
  .attr('st', 'axisRed')
  .attr("transform", "translate(0," + height + ")")
  .call(d3.axisBottom(x));

x_axis.selectAll("line").style("stroke", "white");
x_axis.selectAll('text').style('stroke', 'white');
x_axis.selectAll('path').style('stroke', 'white');

var beta_dist = gen_beta_dist(parseFloat(document.getElementById('alpha_num').value), 
  parseFloat(document.getElementById('beta_num').value));

// add the y Axis
var y = d3.scaleLinear()
  .range([height, 0])
  .domain([0, jStat.max(beta_dist.map(function(x) {return x[1]})) * 1.2]);
var posterior_y_axis = posterior_svg.append("g")
  .call(d3.axisLeft(y));

posterior_y_axis.selectAll("line").style("stroke", "white");
posterior_y_axis.selectAll('text').style('stroke', 'white');
posterior_y_axis.selectAll('path').style('stroke', 'white');


// Plot the area
var posterior = posterior_svg.append("path")
  // .attr("class", "mypath")
  .datum(beta_dist)
  .attr("fill", "rgba(198, 45, 205, 0.8)")
  .attr("opacity", ".8")
  .attr("stroke", "#000")
  .attr("stroke-width", 1)
  .attr("stroke-linejoin", "round")
  .attr("d", d3.area()
    .curve(d3.curveBasis)
    .x(function (d) { return x(d[0]); })
    .y1(function (d) { return y(d[1]); })
    .y0(y(0))
  );

//////////////////// POSTERIOR PREDICTIVE DISTRIBUTION
var posterior_predictive_svg = d3.select("#posterior_predictive_viz")
  .append("svg")
  .attr("width", width + margin.left + margin.right)
  .attr("height", height + margin.top + margin.bottom)
  .append("g")
  .attr("transform",
    "translate(" + margin.left + "," + margin.top + ")");

var posterior_predictive_dist = gen_beta_binomial_dist(parseFloat(document.getElementById('n_num').value), 
  parseFloat(document.getElementById('alpha_num').value), 
  parseFloat(document.getElementById('beta_num').value));

var x = d3.scaleBand()
  .range([ 0, width ])
  .domain(posterior_predictive_dist.map(function(x) {return x[0];}))
  .padding(0.2);

posterior_predictive_x_axis = posterior_predictive_svg.append("g")
  .attr("transform", "translate(0," + height + ")")
  .call(d3.axisBottom(x));

posterior_predictive_x_axis.selectAll("line").style("stroke", "white");
posterior_predictive_x_axis.selectAll('text').style('stroke', 'white');
posterior_predictive_x_axis.selectAll('path').style('stroke', 'white');

var y = d3.scaleLinear()
  .domain([0, jStat.max(posterior_predictive_dist.map(function(x) {return x[1];}))*1.2])
  .range([ height, 0]);
posterior_predictive_y_axis = posterior_predictive_svg.append("g")
  .call(d3.axisLeft(y));

posterior_predictive_y_axis.selectAll("line").style("stroke", "white");
posterior_predictive_y_axis.selectAll('text').style('stroke', 'white');
posterior_predictive_y_axis.selectAll('path').style('stroke', 'white');

var u = posterior_predictive_svg.selectAll("rect")
  .data(posterior_predictive_dist);

u.enter()
.append("rect")
.merge(u)
  .attr("x", function(d) { return x(d[0]); })
  .attr("y", function(d) { return y(d[1]); })
  .attr("width", x.bandwidth())
  .attr("height", function(d) { return height - y(d[1]); })
  .attr("fill", "rgba(198, 45, 205, 0.8)")

//////////////////// Functions
// Approximate Gamma value, accurate to 8 digits for x > 8
function beta_val(alpha, beta){
  return (math.exp(jStat.gammaln(alpha) + jStat.gammaln(beta) - jStat.gammaln(beta+alpha)));
};

function binomail(n, k){
  return math.factorial(n) / (math.factorial(k) * math.factorial(n-k));
};

// Generate values for the beta binomial distribution
function gen_beta_binomial_dist(n, alpha, beta, min_prob=1e-3){
  var xs = [];
  var ss = 0;
  var count = 0;
  while (ss == 0){
    var prob_val = binomail(n, count) * (beta_val(count+alpha, n-count+beta) / beta_val(alpha, beta));
    if (prob_val > min_prob){
      xs.push([count, prob_val]);
    }
    if (((prob_val < min_prob) && (jStat.sum(xs) > 0.7)) || (count >= n)){
      ss = 1;
    }
    count += 1;
  }
  return xs;
}

// Generate beta values for beta distribution based on alpha and beta
function gen_beta_dist(alpha, beta, n=1000){
  var xs = [];
  for (i=0;i<=1;i+=1/n){
    xs.push([i,jStat.beta.pdf(i, alpha, beta)]);
  };
  return xs;
};

// Parse Text field
function parse_text_area(silent=false){
  var t_area = document.getElementById("post_data");
  var lines = t_area.value.replace(/\r\n/g,"\n").split("\n"); 
  var post_data = [];

  for (i=0;i<lines.length;i++){
    var dat = parseInt(lines[i]);
    if (lines[i] == '') {continue;}
    if (isNaN(dat) || dat > parseFloat(document.getElementById('n_num').value) || dat < 0){
      if(!silent){alert('Please enter non-negative numbers below N and no empty lines.');}
      break
      var post_data = [];
    }
    else {
      post_data.push(dat);
    }
  }
  return(post_data);
}

// Using the data from the text field, get the update alpaha and beta
  // This function should only be called after logical checking of whether function can be plotted has occured
function get_updated_alpha_beta(silent=false){
  var data_from_tarea = parse_text_area(silent);
  var updated_alpha = parseFloat(document.getElementById('alpha_num').value) + jStat.sum(data_from_tarea);
  var updated_beta = parseFloat(document.getElementById('beta_num').value) + 
    (parseFloat(document.getElementById('n_num').value) * data_from_tarea.length - jStat.sum(data_from_tarea));

  return([updated_alpha, updated_beta]);
}

// Update function for PRIOR
function update_prior_chart(){

  var alpha = parseFloat(document.getElementById('alpha_num').value);
  var beta = parseFloat(document.getElementById('beta_num').value);

  if ((alpha > 0 && (beta > 0))){

    //// Update Prior
    var beta_dist = gen_beta_dist(alpha, beta);
    var x = d3.scaleLinear()
    .domain([0, 1])
    .range([0, width]);
    var y = d3.scaleLinear()
    .range([height, 0])
    .domain([0, jStat.max(beta_dist.map(function(x) {return x[1]})) * 1.2]);
    
    prior_y_axis.call(d3.axisLeft(y));
    prior_y_axis.selectAll("line").style("stroke", "white");
    prior_y_axis.selectAll('text').style('stroke', 'white');
    prior_y_axis.selectAll('path').style('stroke', 'white');

    prior.datum(beta_dist)
      .transition()
      .duration(1000)
      .attr("fill", "#69b3a2")
      .attr("opacity", ".8")
      .attr("stroke", "#000")
      .attr("stroke-width", 1)
      .attr("stroke-linejoin", "round")
      .attr("d", d3.area()
        .curve(d3.curveBasis)
        .x(function (d) { return x(d[0]); })
        .y1(function (d) { return y(d[1]); })
        .y0(y(0))
      );
  }
}

// Update function for PRIOR PREDICTIVE
function update_prior_predictive_chart(){
  var alpha = parseFloat(document.getElementById('alpha_num').value);
  var beta = parseFloat(document.getElementById('beta_num').value);
  var n = parseFloat(document.getElementById('n_num').value)

  if ((alpha > 0) && (beta > 0) && (n > 0)){
    var prior_predictive_dist = gen_beta_binomial_dist(n, alpha, beta);

    var y = d3.scaleLinear()
      .domain([0, jStat.max(prior_predictive_dist.map(function(x) {return x[1];}))*1.2])
      .range([ height, 0]);
    prior_predictive_y_axis.call(d3.axisLeft(y));
    prior_predictive_y_axis.selectAll("line").style("stroke", "white");
    prior_predictive_y_axis.selectAll('text').style('stroke', 'white');
    prior_predictive_y_axis.selectAll('path').style('stroke', 'white');
    var x = d3.scaleBand()
      .range([ 0, width ])
      .domain(prior_predictive_dist.map(function(x) {return x[0];}))
      .padding(0.2);

    prior_predictive_x_axis.call(d3.axisBottom(x));
    prior_predictive_x_axis.selectAll("line").style("stroke", "white");
    prior_predictive_x_axis.selectAll('text').style('stroke', 'white');
    prior_predictive_x_axis.selectAll('path').style('stroke', 'white');

    var u = prior_predictive_svg.selectAll("rect")
      .data(prior_predictive_dist);

    u.enter()
      .append("rect")
      .merge(u)
      .transition()
      .duration(1000)
        .attr("x", function(d) { return x(d[0]); })
        .attr("y", function(d) { return y(d[1]); })
        .attr("width", x.bandwidth())
        .attr("height", function(d) { return height - y(d[1]); })
        .attr("fill", "#69b3a2")
    u.exit()
      .remove()
  }
}

// Update function for POSTERIOR
function update_posterior_chart(){
  var updated_a_b = get_updated_alpha_beta();

  if ((updated_a_b[0] > 0 && (updated_a_b[1] > 0))){

    //// Update Posterior
    var beta_dist = gen_beta_dist(updated_a_b[0], updated_a_b[1]);
    var x = d3.scaleLinear()
    .domain([0, 1])
    .range([0, width]);
    var y = d3.scaleLinear()
    .range([height, 0])
    .domain([0, jStat.max(beta_dist.map(function(x) {return x[1]})) * 1.2]);
    
    posterior_y_axis.call(d3.axisLeft(y));
    posterior_y_axis.selectAll("line").style("stroke", "white");
    posterior_y_axis.selectAll('text').style('stroke', 'white');
    posterior_y_axis.selectAll('path').style('stroke', 'white');

    posterior.datum(beta_dist)
      .transition()
      .duration(1000)
      .attr("fill", "rgba(198, 45, 205, 0.8)")
      .attr("opacity", ".8")
      .attr("stroke", "#000")
      .attr("stroke-width", 1)
      .attr("stroke-linejoin", "round")
      .attr("d", d3.area()
        .curve(d3.curveBasis)
        .x(function (d) { return x(d[0]); })
        .y1(function (d) { return y(d[1]); })
        .y0(y(0))
      );
  }
}

// Update function for posterior PREDICTIVE
function update_posterior_predictive_chart(){
  var updated_a_b = get_updated_alpha_beta(silent=true);
  var n = parseFloat(document.getElementById('n_num').value);

  if ((updated_a_b[0] > 0) && (updated_a_b[1] > 0) && (n > 0)){
    var posterior_predictive_dist = gen_beta_binomial_dist(n, updated_a_b[0], updated_a_b[1]);

    var y = d3.scaleLinear()
      .domain([0, jStat.max(posterior_predictive_dist.map(function(x) {return x[1];}))*1.2])
      .range([ height, 0]);
    posterior_predictive_y_axis.call(d3.axisLeft(y));
    posterior_predictive_y_axis.selectAll("line").style("stroke", "white");
    posterior_predictive_y_axis.selectAll('text').style('stroke', 'white');
    posterior_predictive_y_axis.selectAll('path').style('stroke', 'white');
    var x = d3.scaleBand()
      .range([ 0, width ])
      .domain(posterior_predictive_dist.map(function(x) {return x[0];}))
      .padding(0.2);

    posterior_predictive_x_axis.call(d3.axisBottom(x));
    posterior_predictive_x_axis.selectAll("line").style("stroke", "white");
    posterior_predictive_x_axis.selectAll('text').style('stroke', 'white');
    posterior_predictive_x_axis.selectAll('path').style('stroke', 'white');

    var u = posterior_predictive_svg.selectAll("rect")
      .data(posterior_predictive_dist);

    u.enter()
      .append("rect")
      .merge(u)
      .transition()
      .duration(1000)
        .attr("x", function(d) { return x(d[0]); })
        .attr("y", function(d) { return y(d[1]); })
        .attr("width", x.bandwidth())
        .attr("height", function(d) { return height - y(d[1]); })
        .attr("fill", "rgba(198, 45, 205, 0.8)")
    u.exit()
      .remove()
  }
}

// Update all charts
function update_all_charts(){
  update_prior_chart();
  update_prior_predictive_chart();
  update_posterior_chart();
  update_posterior_predictive_chart();
}