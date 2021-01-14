# -*- coding: utf-8 -*-
# Shiping Xu
# CS251 PS1 Problem#1

# global variables
score = 0
color = "blue"

def updateScore(points):
    global score, color
    score += points
    if score < 0:
        score = 0
    color = "green" if points > 0 else "red"

def testUpdateScore(points):
    print "before update => score: {}; color: {}".format(score,color)
    updateScore(points)
    print "after update => score: {}; color: {}".format(score,color)
    print "----------------------------"
    
testUpdateScore(10)
testUpdateScore(-7)
testUpdateScore(0)