---
title:    "Java recipe: Reading a text file"
keywords: ["Java"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/java/reading-a-text-file.md"
---

{{< edit_this_page >}}

# Why
Have you ever wanted to read and extract data from a text file in your Java program? In today's digital world, text files are still widely used for storing data and information. Being able to read them using Java can greatly enhance the functionality of your programs. In this blog post, we will delve into the topic of reading text files in Java and provide you with the necessary tools to do so.

# How To
To read a text file in Java, we will use the `FileReader` and `BufferedReader` classes from the `java.io` package. Let's first create a `File` object that represents the text file we want to read, using its absolute or relative path.

```java
File file = new File("path/to/your/file.txt");
```

Next, we will create a `FileReader` object and pass in the `File` object as a parameter.

```java
FileReader fr = new FileReader(file);
```

We will also create a `BufferedReader` object to wrap around our `FileReader` object.

```java
BufferedReader br = new BufferedReader(fr);
```

Now, we can use the `readLine()` method from the `BufferedReader` class to read each line of the text file and store it in a `String` variable.

```java
String line = br.readLine();
```

We can continue to use the `readLine()` method until it returns `null`, indicating that we have reached the end of the text file.

```java
while(line != null){
  // do something with the line of text
  line = br.readLine();
}
```

Let's put all of this together in a simple example. Suppose we have a text file named "cities.txt" that contains a list of cities, each on a separate line. Our goal is to print out each city in the file on a separate line.

```java
import java.io.*;

public class ReadTextFileExample {
  public static void main(String[] args) {
    try {
      File file = new File("cities.txt");
      FileReader fr = new FileReader(file);
      BufferedReader br = new BufferedReader(fr);
      String line = br.readLine();
      while (line != null) {
        System.out.println(line);
        line = br.readLine();
      }
      br.close();
    } catch(Exception e) {
      System.out.println("Could not read file: " + e.getMessage());
    }
  }
}
```

If our "cities.txt" file contains the following lines:

```
London
Paris
New York
Tokyo
```

The program will output:

```
London
Paris
New York
Tokyo
```

# Deep Dive
Behind the scenes, the `FileReader` and `BufferedReader` classes use an input stream to read the data from the text file. The input stream is then converted into characters using the default character encoding of your system.

It is important to note that when using `FileReader`, the entire file is read at once and stored in the computer's memory. This can be problematic for larger files as it can consume a lot of memory. In these cases, it is recommended to use the `BufferedReader` class to read the file line by line.

Additionally, the `FileReader` and `BufferedReader` classes are not the only ways to read text files in Java. There are other classes and libraries, such as `Scanner` and `Apache Commons IO`, that offer different methods and functionalities for reading text files. It is important to explore and choose the most suitable approach for your specific needs.

# See Also
- [Official Java Documentation on Reading from Files](https://docs.oracle.com/javase/tutorial/essential/io/file.html)
- [Tutorial on Java File Input and Output](https://www.baeldung.com/java-io-file)
- [Overview of Common Java Libraries for File I/O](https://www.geeksforgeeks.org/different-ways-reading-text-files-java/)