---
title:                "Java recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why
Have you ever needed to retrieve information from a text file using Java? Whether it's for data analysis or simply viewing the contents of a file, knowing how to read a text file in Java can be a useful skill to have. In this blog post, we'll explore the steps of reading a text file and provide coding examples to help you get started.

## How To
To start, we'll need to import the necessary Java libraries for handling file input and output:

```Java
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;
```

Next, we'll create a File object and use a Scanner to read the contents of the file. The following code snippet shows how to read a text file named "example.txt":

```Java
File file = new File("example.txt");
Scanner scanner = new Scanner(file);

while(scanner.hasNextLine()){
    String line = scanner.nextLine();
    System.out.println(line);
}

scanner.close();
```

The above code uses a while loop to go through each line in the text file and print it to the console. We use the `hasNextLine()` method to check if there is another line in the file, and the `nextLine()` method to retrieve the line and store it in a String variable. Finally, we close the Scanner to release any resources used.

Next, let's say we want to store the lines of the text file in an array for further processing. We can do this by creating an ArrayList and adding each line to it:

```Java
File file = new File("example.txt");
ArrayList<String> lines = new ArrayList<>();
Scanner scanner = new Scanner(file);

while (scanner.hasNextLine()) {
    lines.add(scanner.nextLine());
}

scanner.close();

// Print out all the lines stored in the ArrayList 
for (String line : lines) {
    System.out.println(line);
}
```

In this example, we create an ArrayList and use the `add()` method to add each line from the text file to it. Then, we can use a for loop to go through the ArrayList and print out each line.

## Deep Dive
Now that we've covered the basics of reading a text file, let's dive a little deeper. When using the `Scanner` class, there are a few important things to keep in mind:

- The `Scanner` class throws a `FileNotFoundException` if the file specified is not found.
- We can use the `useDelimiter()` method to specify a delimiter, such as a comma, to split the text file into different tokens.
- By default, the `Scanner` class uses the newline character `\n` as the delimiter.

Another useful class for reading text files is `BufferedReader`. Unlike `Scanner`, it does not automatically handle different types of data, so we need to use methods like `Integer.parseInt()` to convert strings to integers.

## See Also
- [Java File Input/Output (I/O)](https://www.w3schools.com/java/java_files.asp)
- [Reading and Writing Files in Java](https://www.geeksforgeeks.org/reading-writing-data-files-java/)
- [Java ArrayList Class](https://www.w3schools.com/java/java_arraylist.asp)