---
title:                "Working with CSV"
date:                  2024-01-19
html_title:           "C recipe: Working with CSV"
simple_title:         "Working with CSV"

category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?

Working with CSV, which stands for Comma-Separated Values, means handling data in a plain text format where each line is a data record with fields separated by commas. Programmers dig into CSVs because they're simple to use, widely supported, and perfect for exchanging data between different applications.

## How to:

Let's read and write CSV files in Java using the commonly used `OpenCSV` library. First, add the dependency to your `pom.xml` if you're using Maven.

```xml
<dependency>
    <groupId>com.opencsv</groupId>
    <artifactId>opencsv</artifactId>
    <version>5.6</version> <!-- Check for the latest version -->
</dependency>
```

### Writing a CSV file

```java
import com.opencsv.CSVWriter;
import java.io.FileWriter;
import java.io.IOException;

public class CSVWritingExample {
    public static void main(String[] args) {
        String[] header = {"Name", "Age", "Country"};
        String[] record1 = {"Alice", "24", "USA"};
        String[] record2 = {"Bob", "19", "Canada"};

        try (CSVWriter writer = new CSVWriter(new FileWriter("data.csv"))) {
            writer.writeNext(header);
            writer.writeNext(record1);
            writer.writeNext(record2);
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

### Reading a CSV file

```java
import com.opencsv.CSVReader;
import java.io.FileReader;
import java.io.IOException;
import java.util.List;

public class CSVReadingExample {
    public static void main(String[] args) {

        try (CSVReader reader = new CSVReader(new FileReader("data.csv"))) {
            List<String[]> r = reader.readAll();
            r.forEach(x -> System.out.println(x[0] + ", " + x[1] + ", " + x[2]));
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Sample output after reading:

```
Name, Age, Country
Alice, 24, USA
Bob, 19, Canada
```

## Deep Dive

Historically, CSVs have been used since the early days of personal computing, making them a kind of lingua franca for data interchange. Alternatives like JSON, XML, or even Excel formats may offer more advanced features, but CSV's simplicity ensures its endurance. When working with Java, while `OpenCSV` is a popular choice, you can also use the `java.util.Scanner` or `java.io.BufferedReader` for very basic tasks, though you'd handle the parsing yourself. `Apache Commons CSV` is another potent library available for similar tasks.

## See Also

- The OpenCSV homepage for documentation and guides: http://opencsv.sourceforge.net/
- Apache Commons CSV for an alternative approach: https://commons.apache.org/proper/commons-csv/
- Oracle's official Java tutorials for I/O operations: https://docs.oracle.com/javase/tutorial/essential/io/
