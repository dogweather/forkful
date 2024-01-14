---
title:                "Arduino recipe: Working with csv"
simple_title:         "Working with csv"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/working-with-csv.md"
---

{{< edit_this_page >}}

## Why

If you are interested in data analysis or want to incorporate data into your Arduino projects, working with CSV files can be incredibly beneficial. CSV (Comma Separated Values) files are a commonly used format for storing and organizing large amounts of data, making it easier to analyze and process. With Arduino, you can easily retrieve data from CSV files and use it in your projects, adding a whole new level of functionality.

## How To

To work with CSV files in Arduino, you will need the Arduino IDE and the ArduinoCSV library. The following code block shows a simple example of how to retrieve data from a CSV file and print it on the serial monitor.

```Arduino
// include the required library
#include <ArduinoCSV.h>

void setup() {

  // initialize serial communication
  Serial.begin(9600);

  // create an instance of the ArduinoCSV class
  ArduinoCSV csv;

  // open the CSV file
  csv.open("data.csv");

  // read each line and print it on the serial monitor
  while(csv.available()) {
    Serial.println(csv.read());
  }

  // close the CSV file
  csv.close();
}

void loop() {
  // nothing to do here
}
```

Assuming your CSV file is in the same folder as your sketch, you should see the data being printed on the serial monitor. The output will include the headers and values from each row in the CSV file. This is just a basic example, but it demonstrates how easy it is to retrieve data from CSV files in Arduino.

## Deep Dive

One important thing to note when working with CSV files in Arduino is that the data is stored as a string. This means that if you want to use the data as numbers, you will need to convert them using the appropriate functions. For example, if your CSV file contains temperature readings, you will need to use the `toFloat()` function to convert the string into a float value.

Another useful feature of the ArduinoCSV library is the ability to specify the delimiter (i.e. the character that separates each value in the CSV file). By default, it is set to a comma, but you can change it to suit your needs. For example, if your CSV file uses a semicolon as a delimiter, you can specify it by using the `setDelimiter()` function before opening the CSV file.

## See Also

To learn more about working with CSV files in Arduino, check out these resources:

- [ArduinoCSV library documentation](https://github.com/rodan/dueflashstorage)
- [Tutorial on using CSV files in Arduino projects](https://arduinogetstarted.com/library/arduino-csv-files)