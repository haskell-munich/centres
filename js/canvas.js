
var width = 800;
var height = 500;

var vertices = [];

var c = "Red";

var p0 = [30, height-30, 15];
var p1 = [70, height-30, 15];

var svg = d3.select("body").append("svg")
    .attr("width", width)
    .attr("height", height)
    .on("click", function (d, i) { appendCircle(d3.mouse(this)); });

var spacing = 140;

var buttons =
    [ { "id"  : "Red",
	"func": function (d) { c = "Red"; } },

      { "id"  : "Green",
	"func": function (d) { c = "Green"; } },

      { "id"  : "Centre",
	"func": centres },

      { "id"  : "Convex Hull",
	"func": convexhull},

      { "id"  : "Perceptron",
	"func": perceptron} ];

svg.selectAll("rect")
    .data(buttons)
    .enter()
    .append("rect")
    .attr("class", "button")
    .attr("id", function (d) { return d.id; })
    .attr("x", function (d, i) { return 20+spacing*i; })
    .attr("y", height-45)
    .attr("width", spacing-20)
    .attr("height", 30)
    .attr("rx", "10")
    .on("click", function (d) { d.func(); updateButtons() })

svg.selectAll("text")
    .data(buttons)
    .enter()
    .append("text")
    .attr("x", function (d, i) { return 30+spacing*i; })
    .attr("y", height-25)
    .text(function (d) { return d.id; })
    .on("click", function (d) { d.func(); updateButtons() })

svg.append("clipPath")
    .attr("id", "cpath")
    .append("rect")
    .attr("x", 0)
    .attr("y", 0)
    .attr("width", width)
    .attr("height", height - 60)

var g = svg.append("g");
var buttons = svg.selectAll(".button").data(buttons);

updateButtons();

function updateButtons () {
    buttons.attr("stroke-width", function (d) { if (d.id == c) { return 5; } })
	.attr("stroke", function (d) { if (d.id == c) { return "#0000ff"; } });

}

function centres() {
    $.ajax({
	type: "GET",
	url: "http://localhost:8000",
	data: ("cmd=centres&vertices=" + JSON.stringify(vertices)),
	success: function(response) {
	    var centres = JSON.parse(response);

	    var sel = g.selectAll("circle").data(centres);

	    sel.enter()
		.append("circle")
		.attr("class", function (d) { return d.colour + "-centre"; })
		.attr("cx", function (d) { return d.x; })
		.attr("cy", function (d) { return d.y; })
		.attr("r", 20);

	    sel.attr("class", function (d) { return d.colour + "-centre"; })
		.attr("cx", function (d) { return d.x; })
		.attr("cy", function (d) { return d.y; });


	}
    });
}

var lineFunction = d3.svg.line()
    .x(function(d) { return d.x; })
    .y(function(d) { return d.y; })
    .interpolate("linear");

function convexhull() {
    $.ajax({
	type: "GET",
	url: "http://localhost:8000",
	data: ("cmd=convexhull&vertices=" + JSON.stringify(vertices)),
	success: function(response) {
	    var paths = JSON.parse(response);
	    var sel = g.selectAll("#convex-hull").data(paths);


	    sel.enter()
		.append("path")
		.attr("id", "convex-hull")
		.attr("class",
		      function (d) { if (d.length > 0) { return d[0].colour + "-centre" }})
	    	.attr("d", lineFunction);

	    sel .attr("class",
		      function (d) { if (d.length > 0) { return d[0].colour + "-centre" }})
	    	.attr("d", lineFunction);

	}
    });
}

function perceptron() {
    $.ajax({
	type: "GET",
	url: "http://localhost:8000",
	data: ("cmd=perceptron&vertices=" + JSON.stringify(vertices)),
	success: function(response) {
	    var pts = JSON.parse(response);

	    var sel = g.selectAll(".separation-line").data([pts]);

	    sel.enter()
		.append("path")
		.attr("class", "separation-line")
		.attr("d", lineFunction)
		.attr("clip-path", "url(#cpath)");

	    sel.attr("d", lineFunction);
	}
    });
}

function appendCircle(n) {
    var pt = { x:n[0], y:n[1], colour:c };

    if (n[1] < height - 60) {
	vertices.push(pt);

	svg.append("circle")
	    .attr("class", c)
	    .attr("cx", n[0])
	    .attr("cy", n[1])
	    .attr("r", 4); }; }
