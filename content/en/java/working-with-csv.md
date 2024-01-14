---
title:                "Java recipe: Working with csv"
simple_title:         "Working with csv"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/working-with-csv.md"
---

{{< edit_this_page >}}

## Why
CSV (Comma-Separated Values) is a commonly used file format for storing and exchanging data. Many popular programs like Excel, Google Sheets, and databases support CSV files. Learning how to work with CSV can greatly improve your data management skills and make your programming tasks more efficient.

## How To
Working with CSV files in Java is fairly straightforward and can be done using built-in libraries. Let's take a look at an example of reading data from a CSV file and printing it out.

```Java
import java.io.File;
import java.io.FileNotFoundException;
import java.util.Scanner;

public class CSVReader {
    public static void main(String[] args) {
        try {
            // Create a File object with the filepath of the CSV file
            File csvFile = new File("data.csv");

            // Create a Scanner object to read the CSV file
            Scanner scanner = new Scanner(csvFile);

            // Loop through each line of the CSV file
            while (scanner.hasNextLine()) {

                // Use the comma as the delimiter to separate each value in the line
                String[] values = scanner.nextLine().split(",");

                // Print out the values from each line
                System.out.println("Name: " + values[0]);
                System.out.println("Age: " + values[1]);
                System.out.println("City: " + values[2]);
            }

            // Close the Scanner object
            scanner.close();
        } catch (FileNotFoundException e) {
            // Catch any errors, such as the CSV file not being found
            System.out.println("File not found.");
        }
    }
}
```
Output:
```
Name: John
Age: 28
City: New York
```

In this example, we first import the necessary libraries and then create a File object with the filepath of our CSV file. We then use the Scanner class to read the file, loop through each line, and split the values using a comma as the delimiter. Finally, we print out the values for each line.

To write data to a CSV file, we can use the FileWriter class. Let's take a look at another example:

```Java
import java.io.FileWriter;
import java.io.IOException;

public class CSVWriter {
    public static void main(String[] args) {
        try {
            // Create a FileWriter object with the filepath of the CSV file
            FileWriter writer = new FileWriter("data.csv", true);

            // Write the data to the CSV file
            writer.write("Sarah,33,Chicago\n");
            writer.write("David,25,Los Angeles\n");

            // Close the FileWriter object
            writer.close();
        } catch (IOException e) {
            // Catch any errors, such as the file not being found or being unable to write to it
            System.out.println("Error writing to file.");
        }
    }
}
```

In this example, we use the FileWriter class to write data to our CSV file. The `true` parameter in the constructor allows us to append data to the existing file if it already exists.

## Deep Dive
CSV files have a variety of formats, and it's important to understand how to handle different types of data. For example, if your CSV file includes a header row with labels for each column, you can use the `scanner.nextLine()` to skip over it and start reading data from the next line.

It's also important to keep in mind that CSV files may contain data that needs to be formatted or converted in some way. For example, if a value is in a different date format or contains unnecessary spaces, you may need to use methods like `trim()` or `DateFormat` to properly format the data.

## See Also
- [Java CSV Tutorial](https://www.baeldung.com/java-csv)
- [Working with CSV files in Java](https://www.codejava.net/java-se/file-io/parse-csv-files-in-java)
- [CSV file format](https://en.wikipedia.org/wiki/Comma-separated_values)