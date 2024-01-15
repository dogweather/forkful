---
title:                "Working with csv"
html_title:           "Java recipe: Working with csv"
simple_title:         "Working with csv"
programming_language: "Java"
category:             "Java"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/java/working-with-csv.md"
---

{{< edit_this_page >}}

## Why

It's likely that at some point in your programming career, you will encounter CSV files. CSV (Comma-Separated Values) is a common file format used for storing and exchanging tabular data. As a Java programmer, understanding how to work with CSV files can be a valuable skill to have in your toolkit.

## How To

Working with CSV files in Java is made easy with the help of libraries like OpenCSV and Apache Commons CSV. Let's take a look at some coding examples to see how we can read and write CSV files using these libraries.

### Reading CSV files with OpenCSV
```Java
CSVReader reader = new CSVReader(new FileReader("data.csv"));
String[] nextLine;

while ((nextLine = reader.readNext()) != null) {
    // nextLine contains array of values from each row
    System.out.println(Arrays.toString(nextLine));
}

reader.close();
```

In this example, we use the CSVReader class from the OpenCSV library to read in a CSV file named "data.csv". Each row of the file is represented as an array of String values, which we can access and manipulate as needed. 

### Writing CSV files with Apache Commons CSV
```Java
CSVFormat csvFormat = CSVFormat.DEFAULT.withHeader("Name", "Age", "City");
CSVPrinter printer = new CSVPrinter(new FileWriter("output.csv"), csvFormat);

// Adding records to the CSV file
printer.printRecord("John", 32, "New York");
printer.printRecord("Sarah", 27, "London");
printer.printRecord("Alex", 40, "Sydney");

printer.close();
```

In this example, we use the CSVPrinter class from the Apache Commons CSV library to create a new CSV file named "output.csv". We specify the headers of the file using the CSVFormat class, and then add records to the file using the printRecord() method. The resulting file will have a header row followed by three data rows.

## Deep Dive

CSV files may seem simple, but there are some important things to keep in mind when working with them. Here are a few tips for dealing with CSV files in Java:

- Remember to handle any exceptions that may occur when working with CSV files. This means using try-catch blocks or throwing the exceptions to be handled by the calling code.
- Beware of characters that may cause issues with CSV file parsing, such as commas or line breaks. They may need to be escaped or replaced to avoid errors.
- Take note of the encoding of your CSV files. Depending on the source of the file, it may be encoded in a different format (e.g. UTF-8, ISO-8859-1) which can affect how it is read or written in Java.

## See Also

- [OpenCSV tutorial](https://www.baeldung.com/opencsv)
- [Apache Commons CSV tutorial](https://www.baeldung.com/apache-commons-csv)
- [Oracle's Working with CSV Files in Java guide](https://docs.oracle.com/javase/8/docs/api/javax/swing/text/csv/package-summary.html)