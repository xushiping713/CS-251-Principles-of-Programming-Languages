// global variables
var score = 0;
var color = "blue";

function updateScore(points) {
	score += points;
	if (score < 0) {
		score = 0;
	}
	color = points > 0 ? "green" : "red"
}

function testUpdateScore(points) {
	console.log("before update => score: " + score + "; color: " +color);
	updateScore(points);
	console.log("after update => score: " + score + "; color: " +color);
	console.log("-------------------");
}

testUpdateScore(10)
testUpdateScore(-5)
testUpdateScore(0)