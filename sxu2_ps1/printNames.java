/* printNames.java
 * CS251 PS1 Problem#2
 * Written by: Shiping Xu
 * 09/14/2017
 */

import java.io.*; // Needed to use File and FileNotFoundException classes
import java.util.*; // Needed for classes like Scanner, ArrayList, HashMap, Collection, etc. 

/** 
 * A Java class with a static doPrintNames method that acts like the printNames
 * function in printNames.py. 
 * 
 * Invoking the main method of this class on command line arguments behaves like
 * invoking printNames.py on command line arguments.
 *
 * This class does *not* have have any instance variales or methods or 
 * (nontrivial) constructor methods. It is not expected that instances of this 
 * class will be created. Rather, this class is just a respository for 
 * the doPrintNames static method corresponding to the Python printNames function. 
 * 
 */

public class printNames {

  /**
   * This Java static method should have the same behavior as the Python printNames function
   * in printNames.py.
   *
   * @param  filename The relative pathname of a file of first and last names
   * @param  numEntries A positive integer indicating the number of entries to be displayed
   *
   */
  public static void doPrintNames(String filename, int numEntries) throws FileNotFoundException {
    Scanner reader = new Scanner(new File(filename)); // reader is object that reads lines of file
    HashMap<String, ArrayList<String>> nameDict = new HashMap<String, ArrayList<String>>();
    // nameDict is dictionary associating first name with list of all last names it appears
    // with in file (including duplicates)
    while (reader.hasNextLine()) {
      String line = reader.nextLine();
      String [] names = line.split("\\s+"); // split on whitespace
      String first = names[0];
      String last = names[1];
      ArrayList<String> lastNameList; 
      if (!nameDict.containsKey(first)) {
        // Create empty lastNameList and associated it with first name key
        lastNameList = new ArrayList<String>(); 
        nameDict.put(first,lastNameList); // put firstname-lastnamelist pair into nameDict
      } else {
        // Find existing lastNameList associated with first name key
        lastNameList = nameDict.get(first); 
      }
      // Add last name to lastNameList associated with first name key
      lastNameList.add(last);
    }

    // Convert the set of entries into an ArrayList so that we can use Collections.sort on it. 
    List<Map.Entry<String,ArrayList<String>>> entriesList
      = new ArrayList<Map.Entry<String,ArrayList<String>>>(nameDict.entrySet());
    
    // Sort entriesList based on the total length of last names comes with the first name in input file
    Collections.sort(entriesList, new Comparator<Map.Entry<String,ArrayList<String>>>() {
      public int compare(Map.Entry<String,ArrayList<String>> o1, Map.Entry<String,ArrayList<String>> o2) {
        return totalLength(o2.getValue()) - totalLength(o1.getValue());
      }
    });

    // for the first "numEntries" number of entries in the list, sort the last name list of each first name
    // according to length, if length are the same, sort by alphabetical order
    for (Map.Entry<String,ArrayList<String>> entry : entriesList.subList(0, Math.max(0, numEntries))) {
      Collections.sort(entry.getValue(), new Comparator<String>() {
        public int compare(String o1, String o2) {
          if (o1.length() == o2.length()) {
            return o1.compareTo(o2);
          }
          return o1.length() - o2.length();
        }
      });
      
      // print out each first name and a numbered last name list in the correct format 
      System.out.println("--------\n" + entry.getKey() + " (" + totalLength(entry.getValue()) + "):");
      int index = 0;     
      for (String lastName : entry.getValue()) {
        System.out.println(((index++)+1) + ". " + lastName);
      } 
    }
}
  // helper method that returns the sum of string length in a list
  private static int totalLength(ArrayList<String> strings) {
    int totalLen = 0;
    for (String string : strings) {
      totalLen += string.length();
    }
    return totalLen;
  }
    
  public static void main (String[] args) {
    if (args.length != 2) { 
      System.out.println("Usage: java printNames <namesFile> <numEntries>");
      for (String s : args) {
        System.out.println(s);
      }
    } else {
      String filename = args[0];
      int numEntries = Integer.parseInt(args[1]);
      try {
        doPrintNames(filename, numEntries);
      } catch (FileNotFoundException e) {
        System.out.println("There is no file named " + filename);
      }
    }
  }

}