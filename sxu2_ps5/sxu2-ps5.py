def process(ints):
    acc = 0
    index = 0
    neg = 1 # assign neg to a non-negative number if no negative num encountered
    while (index < len(ints)):
        if neg != 1:
            if ints[index] == neg:
                neg = 1  
            else:
                acc -= ints[index]            
        elif ints[index] == 42:
            return acc
        elif ints[index] < 0:                   
            neg = ints[index]
        elif ints[index] == 0:
            if index != len(ints)-1: 
                skipNum = ints[index+1]
                index += skipNum+1
            else:
                return acc
        else:
            acc += ints[index]
        index += 1
    return acc

def testCase(expected, nums):
    ans = process(nums)
    if expected == ans:
        print "process passed test with answer", expected
    else:
        print "*** ERROR: process got", ans, "but expected", expected

def testAll():
    testCase(15, [1,2,3,4,5])
    testCase(19, [1,7,2,9])
    testCase(15, [1,2,3,4,5,42,6,7])
    testCase(6, [1,2,3,42,4,5,42,6,7])
    testCase(0, [42,1,2,3,4,5,6,7])
    testCase(10, [1,2,3,-17,4,5,-17,6,7])
    testCase(20, [1,2,-1,4,6,-1,7,8,-5,9,-5,10,11])
    testCase(7,[1,2,3,-1,4,-5,6,-1,7,8,-5,9])
    testCase(-35, [1,2,-1,4,42,5,-1,6,7])
    testCase(26, [4,5,0,2,6,7,8,9])
    testCase(14, [7,2,0,3,6,1,8,5,0,4,9,10])
    testCase(10, [7,3,0])
    testCase(2,[7,3,0,4,-1,0,8,42,5,-1,4,9])
    testCase(499999499097, range(43, 1000000))
    testCase(7999997999097, range(43, 4000000))
    testCase(-42, range(43, 1000000) + [-17] + range(0,1000000) + [-17] + range(1,43))
    testCase(-903, range(43, 1000000) + [-17] + range(0,1000000) + [-17, 42] + range(1,43))
    testCase(-581, range(43, 1000000) + [-17] + range(0,1000000) + [-17, 0, 42] + range(1,50))