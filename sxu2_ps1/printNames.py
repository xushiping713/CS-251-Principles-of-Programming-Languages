import sys

def printNames(namesFile, numEntries):
  with open (namesFile, 'r') as inFile: # open namesFile for reading
    nameDict = {} # create an empty dictionary
    for line in inFile.readlines(): 
      # associate each first name with a list of last names
      first, last = line.split()
      if first not in nameDict:
        nameDict[first] = [last]
      else:
        nameDict[first].append(last)
    # Below, an "item" is a pairof (1) a first name and(2) an associated list of last names
    sortedItems= sorted(nameDict.items(),
                         # How does the following key function sort the items?
                         key=lambda (first,lasts): totalLength(lasts),
                         reverse=True) # Sort items in descending rather than ascending order by key.
    for (fst,lastList) in sortedItems[:max(0,numEntries)]: # what is max doing here?
      sortedLasts = sorted(lastList,
                           # How does the following key function sort the last namesin lastList?
                           # Note that the key returns a *pair* of string length and string. Why?
                           key=lambda s: (len(s),s))
      # Use "list comprehension" notation (in brackets) to create a list of numbered lines.
      numberedLines = [str(i+1) + '. ' + s for i,s in enumerate(sortedLasts)]
      print '--------\n' + fst + ' (' + str(totalLength(sortedLasts)) + '):\n' \
        + '\n'.join(numberedLines)

def totalLength(strings):
  return sum([len(s) for s in strings])

if __name__ == "__main__":
  if len(sys.argv) != 3:
    print "Usage: python printNames.py <namesFile> <numEntries>"
  else:
    # All kinds of errors can occur here, but don't worry about exception handling
    printNames(sys.argv[1], int(sys.argv[2]))