---
date: 2024-02-03 19:03:27.488025-07:00
description: 'How to: Java does not have built-in support for CSV in its standard
  library, but you can easily read a CSV file using `java.io` classes.'
lastmod: '2024-03-13T22:44:59.991798-06:00'
model: gpt-4-0125-preview
summary: Java does not have built-in support for CSV in its standard library, but
  you can easily read a CSV file using `java.io` classes.
title: Working with CSV
weight: 37
---

## How to:


### Reading a CSV file using the standard Java library
Java does not have built-in support for CSV in its standard library, but you can easily read a CSV file using `java.io` classes.

```java
import java.io.BufferedReader;
import java.io.FileReader;
import java.io.IOException;

public class ReadCSVExample {
    public static void main(String[] args) {
        String line;
        String csvFile = "data.csv"; // Specify the path to the CSV file
        try (BufferedReader br = new BufferedReader(new FileReader(csvFile))) {
            while ((line = br.readLine()) != null) {
                String[] values = line.split(","); // Assuming a comma is the delimiter
                // Process the data
                for (String value : values) {
                    System.out.print(value + " ");
                }
                System.out.println();
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

### Writing to a CSV file using the standard Java library
To write data to a CSV file, you can use `java.io` classes such as `FileWriter` and `BufferedWriter`.

```java
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

public class WriteCSVExample {
    public static void main(String[] args) {
        String[] data = {"John", "Doe", "30", "New York"};
        String csvFile = "output.csv"; // Specify the output CSV file path

        try (BufferedWriter bw = new BufferedWriter(new FileWriter(csvFile))) {
            StringBuilder sb = new StringBuilder();
            for (String value : data) {
                sb.append(value).append(","); // Assuming a comma is the delimiter
            }
            sb.deleteCharAt(sb.length() - 1); // Remove the last comma
            bw.write(sb.toString());
            bw.newLine();
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

### Using a third-party library: Apache Commons CSV
Apache Commons CSV is a popular library for handling CSV files in Java. It simplifies reading and writing CSV files significantly.

Add the dependency to your project:

For Maven:

```xml
<dependency>
    <groupId>org.apache.commons</groupId>
    <artifactId>commons-csv</artifactId>
    <version>1.9.0</version> <!-- Check for the latest version -->
</dependency>
```

#### Reading a CSV file:
```java
import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;

import java.io.Reader;
import java.io.FileReader;
import java.io.IOException;

public class ApacheReadCSVExample {
    public static void main(String[] args) {
        String csvFile = "data.csv";
        try (Reader reader = new FileReader(csvFile);
             CSVParser csvParser = new CSVParser(reader, CSVFormat.DEFAULT)) {
            for (CSVRecord csvRecord : csvParser) {
                // Accessing values by the indexes of columns
                String columnOne = csvRecord.get(0);
                String columnTwo = csvRecord.get(1);
                System.out.println(columnOne + " " + columnTwo);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

#### Writing to a CSV file:
```java
import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVPrinter;

import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;

public class ApacheWriteCSVExample {
    public static void main(String[] args) {
        String[] headers = {"First Name", "Last Name", "Age", "City"};
        String[] data = {"John", "Doe", "30", "New York"};

        try (BufferedWriter writer = new BufferedWriter(new FileWriter("output.csv"));
             CSVPrinter csvPrinter = new CSVPrinter(writer, CSVFormat.DEFAULT.withHeader(headers))) {
            csvPrinter.printRecord((Object[]) data); // Casting to Object[] is necessary here
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}
```

Apache Commons CSV handles complexities such as quotations and commas within fields automatically, making it a robust choice for CSV manipulation in Java.
