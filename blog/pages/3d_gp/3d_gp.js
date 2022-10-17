var btn_state = false;
var zPts = []; 
var xPts = [];
var yPts = [];
var t = 0;
var t_step = 0.1;
var xy_step = 0.2;
var x_min = -5;
var x_max = 5;
var y_min = -5;
var y_max = 5;



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

var data = [{
    z: zPts,
    x: xPts,
    y: yPts,
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

function compute(){
    var counter = 0;
    for(x=x_min; x<x_max; x+=xy_step) {
        let zTemp = [];
        let yTemp = [];
        let xTemp = [];
        for (y=y_min; y<y_max; y+=xy_step) {
            zTemp.push(Math.cos(Math.sqrt(x**2 + y**2) + t));
            yTemp.push(y);
            xTemp.push(x);
        }
        zPts[counter] = zTemp;
        yPts[counter] = yTemp;
        xPts[counter] = xTemp;
        counter ++;
    }

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

function parse_text_area(silent=false){
    var t_area = document.getElementById("post_data");
    var lines = t_area.value.replace(/\r\n/g,"\n").split("\n"); 
    var post_data = [];

    for (i=0;i<lines.length;i++){
        var dat = lines[i].slice(lines[i].indexOf('(')+1, lines[i].indexOf(')')).split(',').map(x => parseInt(x));
        post_data.push(dat);
    }
    return(post_data);
}

observations = parse_text_area();