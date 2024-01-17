---
title:                "Working with csv"
html_title:           "Arduino recipe: Working with csv"
simple_title:         "Working with csv"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why?

Working with CSV (Comma Separated Values) in programming allows us to store and manipulate data in a table format. It is a common method for organizing and transferring data between different systems. Programmers use CSV because it is a lightweight and easy-to-read format that can be easily exported, imported, and shared.

## How to:

To work with CSV in Arduino, we first need to download and install the CSV library. This can be done by going to Sketch > Include Library > Manage Libraries and searching for "CSV." Once the library is installed, we can start using it in our code.

### Reading CSV

To read data from a CSV file, we first need to create an instance of the CSV object and pass in the name of the file we want to read from. Then, we can use the ```nextColumn()``` and ```getInt()``` functions to read data from each column and convert it to an integer, respectively. See the example code below:

```
#include<CSV.h>

// Create CSV object and pass in file name
CSV data("data.csv");

void setup() {
    // Open serial connection for output
    Serial.begin(9600);
}

void loop() {
    // Read data from each column and convert to integer
    int col1 = data.nextInt();
    int col2 = data.nextInt();

    // Print data to serial monitor
    Serial.print("Column 1: ");
    Serial.println(col1);
    Serial.print("Column 2: ");
    Serial.println(col2);
}
```

### Writing CSV

To write data to a CSV file, we can use the ```addNewRow()``` and ```writeField()``` functions. The ```addNewRow()``` function creates a new row in the CSV file, and the ```writeField()``` function adds data to a specific column in the row. See the example code below:

```
#include<CSV.h>

// Create CSV object and pass in file name
CSV data("data.csv");

void setup() {
    // Open serial connection for input
    Serial.begin(9600);
}

void loop() {
    // Get user input
    int col1 = Serial.parseInt();
    int col2 = Serial.parseInt();

    // Add new row and write data to columns
    data.addNewRow();
    data.writeField(col1);
    data.writeField(col2);
}
```

## Deep Dive:

CSV has been around since the early days of personal computing and was created to allow data to be easily shared between different spreadsheet programs. Other alternatives to CSV for organizing tabular data include JSON and XML. CSV is often preferred due to its simplicity and smaller file size.

When working with CSV in Arduino, it is important to keep in mind the limitations of the board's memory. If working with large amounts of data, it may be necessary to split the data into multiple CSV files or use a different format. Additionally, it is important to ensure that the CSV file being read has a consistent format, as any formatting errors may cause issues with reading the data correctly.

## See Also:

- Arduino CSV library documentation: https://github.com/jrockowitz/arduino-csv
- Tutorial on reading and writing CSV in Arduino: https://create.arduino.cc/projecthub/ReanimationXP/how-to-read-write-csv-data-into-arduino-ea12d4