function rand_num(low=0, high=5){
    return Math.floor(Math.random() * (high - low) ) + low;
}
var xs = []
var ys = []


var ctx = document.getElementById("MyChart").getContext('2d');
var myChart = new Chart(ctx, {
type: 'line',
options: {

        scales: {
            yAxes: [{
                ticks: {
                    fontColor: "white",
                    fontSize: 14,
                    beginAtZero: true
                }
            }],
            xAxes: [{
                ticks: {
                    fontColor: "white",
                    fontSize: 14
                }
            }]
        },
	responsive:true,
	maintainAspectRatio:false,
    legend: {display: false}

},
data: {
    labels: "",
    datasets: [
    {
        label: false,
        data: "",
        borderWidth: 1,
    }, ]
},
});


//////////// Functions
// Periodic Functionality
function periodic(low=-15, high=15, chartObj=myChart){
    var xs = [];
    var ys = [];
    var k = rand_num(3,8);
    var A = rand_num(1,4);
    var dis = rand_num(1,8);
    console.log(k);
    for (i=low; i<=high; i++){
        xs.push(i);
        var yUp = A * Math.sin(i/k - dis);
        var noise = rand_num(-Math.floor(yUp * 0.30), Math.floor(yUp*0.30));
        ys.push(yUp);
    }




    var ctx = document.getElementById("MyChart").getContext('2d');
    var myChart = new Chart(ctx, {
    type: 'line',
    options: {

        scales: {
            yAxes: [{
                ticks: {
                    fontColor: "white",
                    fontSize: 14,
                    beginAtZero: true
                }
            }],
            xAxes: [{
                ticks: {
                    fontColor: "white",
                    fontSize: 14
                }
            }]
        },
        responsive:true,
        maintainAspectRatio:false,
        legend : {
            labels: {
                fontColor: 'white',
            },
        }

    },
    data: {
        labels: xs,
        datasets: [
        {
            label: 'Periodic',
            data: ys,
            borderWidth: 1,
            fill: false, 
            showLine: false,
            backgroundColor: 'rgba(1,1,1,0)',
            pointBackgroundColor: 'rgba(1,1,1,0)',
            borderColor: 'white',
        }, ]
    },
    });
    return [xs, ys, 'Periodic'];
}




// Cubic Functionality
function cube(low=-15, high=15, chartObj=myChart){
    var xs = [];
    var ys = [];
    for (i=low; i<=high; i++){
        xs.push(i);
        var yUp = i**3;
        var noise = rand_num(-Math.floor(yUp * 0.30), Math.floor(yUp*0.30));
        ys.push(noise + yUp);
    }




    var ctx = document.getElementById("MyChart").getContext('2d');
    var myChart = new Chart(ctx, {
    type: 'line',
    options: {

        scales: {
            yAxes: [{
                ticks: {
                    fontColor: "white",
                    fontSize: 14,
                    beginAtZero: true
                }
            }],
            xAxes: [{
                ticks: {
                    fontColor: "white",
                    fontSize: 14
                }
            }]
        },
        responsive:true,
        maintainAspectRatio:false,
        legend : {
            labels: {
                fontColor: 'white',
            },
        }

    },
    data: {
        labels: xs,
        datasets: [
        {
            label: 'Cubic Noise',
            data: ys,
            borderWidth: 1,
            fill: false, 
            showLine: false,
            backgroundColor: 'rgba(1,1,1,0)',
            pointBackgroundColor: 'rgba(1,1,1,0)',
            borderColor: 'white',
        }, ]
    },
    });
    return [xs, ys, 'Cubic'];
}

//Quadratic Function
function quadratic(low=-15, high=15, chartObj=myChart){
    var xs = [];
    var ys = [];
    for (i=low; i<=high; i++){
        xs.push(i);
        var yUp =  i**2 + i;
        var noise = rand_num(-Math.floor(yUp * 0.30), Math.floor(yUp*0.30));
        ys.push(noise + yUp);
    }




    var ctx = document.getElementById("MyChart").getContext('2d');
    var myChart = new Chart(ctx, {
    type: 'line',
    options: {

        scales: {
            yAxes: [{
                ticks: {
                    fontColor: "white",
                    fontSize: 14,
                    beginAtZero: true
                }
            }],
            xAxes: [{
                ticks: {
                    fontColor: "white",
                    fontSize: 14
                }
            }]
        },
        responsive:true,
        maintainAspectRatio:false,
        legend : {
            labels: {
                fontColor: 'white',
            },
        }

    },
    data: {
        labels: xs,
        datasets: [
        {
            label: 'Quadratic Noise',
            data: ys,
            borderWidth: 1,
            fill: false, 
            showLine: false,
            backgroundColor: 'rgba(1,1,1,0)',
            pointBackgroundColor: 'rgba(1,1,1,0)',
            borderColor: 'white',
        }, ]
    },
    });
    return [xs, ys, 'Quadratic'];
}

// Linear Function
function linear(low=-15, high=15, chartObj=myChart){
    var xs = [];
    var ys = [];
    for (i=low; i<=high; i++){
        xs.push(i);
        var yUp = i;
        var noise = rand_num(-Math.floor(yUp * 0.30), Math.floor(yUp*0.30));
        ys.push(noise + yUp);
    }




    var ctx = document.getElementById("MyChart").getContext('2d');
    var myChart = new Chart(ctx, {
    type: 'line',
    options: {

        scales: {
            yAxes: [{
                ticks: {
                    fontColor: "white",
                    fontSize: 14,
                    beginAtZero: true
                }
            }],
            xAxes: [{
                ticks: {
                    fontColor: "white",
                    fontSize: 14
                }
            }]
        },
        responsive:true,
        maintainAspectRatio:false,
        legend : {
            labels: {
                fontColor: 'white',
            },
        }
    },
    data: {
        labels: xs,
        datasets: [
        {
            label: 'Linear Noise',
            data: ys,
            borderWidth: 1,
            fill: false, 
            showLine: false,
            backgroundColor: 'rgba(1,1,1,0)',
            pointBackgroundColor: 'rgba(1,1,1,0)',
            borderColor: 'white',
        }, ]
    },
    });
    return [xs, ys, 'Linear'];
}
///////////////

//Training the model
async function train(xs, ys){
    var ep = document.getElementById('epochs').value;
    if (ep === ""){
        alert('Pick a training iteration between 1 and 899.')
    }

    else if (xs === undefined || xs.length == 0){
        alert('Please select a function before training.')
    }

    else if (ep >= 900){
        alert('That training iteration is TOO BIG, pick something smaller than 900. Thanks!')
    }

    else {
        console.log(xs)
        const model = tf.sequential()
        model.add(tf.layers.dense({units:128, inputShape:[1]}));
        model.add(tf.layers.dense({units:128, inputShape:[128], activation:"sigmoid"}));
        model.add(tf.layers.dense({units:1, inputShape:[128]}));

        const optimizer = tf.train.adam(0.05);

        model.compile({loss: "meanSquaredError", optimizer:optimizer});


        model.fit(tf.tensor(xs.map(function(x) {return( (x + 15)/30)})), tf.tensor(ys), {epochs:ep, shuffle:true}).then(() =>{

            var bestfit = model.predict(tf.tensor(xs.map(function(x) {return( (x + 15)/30)}), [xs.length, 1])).dataSync();


            var ctx = document.getElementById("MyChart").getContext('2d');
            var myChart = new Chart(ctx, {
            type: 'line',
            options: {

        scales: {
            yAxes: [{
                ticks: {
                    fontColor: "white",
                    fontSize: 14,
                    beginAtZero: true
                }
            }],
            xAxes: [{
                ticks: {
                    fontColor: "white",
                    fontSize: 14
                }
            }]
        },
                responsive:true,
                maintainAspectRatio:false,
                legend : {
                    labels: {
                        fontColor: 'white',
                    },
                }

            },
            data: {
                labels: xs,
                datasets: [
                {
                    label: name == 'Periodic' ? name: name + ' Noise',
                    data: ys,
                    borderWidth: 1,
                    fill: false, 
                    showLine: false,
                    backgroundColor: 'rgba(1,1,1,0)',
                    pointBackgroundColor: 'rgba(1,1,1,0)',
                    borderColor: 'white',

                }, 
                {
                    label: "Trained Model: " + ep +' Iterations',
                    data: bestfit,
                    borderWidth: 2.5,
                    fill: false,
                    pointRadius: 0,
                    borderColor: "#FF0000",
                    backgroundColor: "rgba(1,1,1,0)"     
                }


                ]
            },
            });




            });



    }
}


