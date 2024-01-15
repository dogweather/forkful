---
title:                "テキストファイルの書き方"
html_title:           "Java: テキストファイルの書き方"
simple_title:         "テキストファイルの書き方"
programming_language: "Java"
category:             "Java"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/java/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why
Writing a text file is a common task in Java programming, as it allows developers to store and manipulate data outside of the code. This can be useful for storing user input, logging information, and more.

## How To
To write a text file in Java, we will first need to import the necessary classes:

```
import java.io.FileWriter;
import java.io.BufferedWriter;
```

Next, we will create a FileWriter and BufferedWriter object to handle the writing process:

```
FileWriter fileWriter = new FileWriter("myFile.txt");
BufferedWriter bufferedWriter = new BufferedWriter(fileWriter);
```

Now, we can start writing to the file using the `write()` method:

```
bufferedWriter.write("Hello, world!");
```

We can also use a for loop to write multiple lines of text:

```
for (int i = 0; i < 5; i++) {
    bufferedWriter.write("This is line " + i);
    bufferedWriter.newLine();
}
```

And don't forget to close the writer to ensure the changes are saved:

```
bufferedWriter.close();
```

Finally, let's take a look at the contents of our file using a FileReader and BufferedReader:

```
import java.io.FileReader;
import java.io.BufferedReader;

FileReader fileReader = new FileReader("myFile.txt");
BufferedReader bufferedReader = new BufferedReader(fileReader);

String line = bufferedReader.readLine();

while (line != null) {
    System.out.println(line);
    line = bufferedReader.readLine();
}

bufferedReader.close();
```

Running this code should output:

```
Hello, world!
This is line 0
This is line 1
This is line 2
This is line 3
This is line 4
```

## Deep Dive
In the above examples, we used the `BufferedWriter` class because it provides buffering for the write operations, making it more efficient for writing larger amounts of data. However, we can also use the `FileWriter` class directly for simpler operations.

Additionally, we can specify the file encoding when creating the `FileWriter` object, to ensure that special characters are written correctly. For example, we can use the UTF-8 encoding as follows:

```
FileWriter fileWriter = new FileWriter("myFile.txt", StandardCharsets.UTF_8);
```

It's also worth noting that both the `FileWriter` and `BufferedWriter` classes have a `append()` method, which can be useful for adding new content to an existing file without overwriting its contents.

## See Also
- [Java FileWriter class documentation](https://docs.oracle.com/javase/10/docs/api/java/io/FileWriter.html)
- [Java BufferedWriter class documentation](https://docs.oracle.com/javase/10/docs/api/java/io/BufferedWriter.html)