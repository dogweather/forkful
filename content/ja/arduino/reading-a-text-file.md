---
title:                "テキストファイルの読み込み"
html_title:           "Arduino: テキストファイルの読み込み"
simple_title:         "テキストファイルの読み込み"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/ja/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Text files are a common way to store data and information, and being able to read them can be useful for a variety of projects. For example, you may want to retrieve data from a sensor or log data for analysis.

## How To

```Arduino
#include <SD.h> //include the SD library
#include <SPI.h> //include the SPI library

File dataFile; //create file object

void setup() {
  Serial.begin(9600); //initialize serial communication at 9600 baud

  //check if SD card is present
  if (!SD.begin(4)) {
    Serial.println("SD card not found.");
    while (true); //halt program
  }

  //open the text file and store it in the file object
  dataFile = SD.open("data.txt");

  //read and print contents of file
  while (dataFile.available()) {
    Serial.write(dataFile.read());
  }
  dataFile.close(); //close the file
}

void loop() {

}
```

After uploading the code, open the serial monitor to see the contents of the text file. The Arduino will read the file line by line and print it out.

Sample output:

```
Temperature: 25 C
Humidity: 50%
Pressure: 101.3 kPa
```

## Deep Dive

The SD library has a built-in function, `open()`, which allows us to open a file and store it in a file object. We can then use the `read()` function to read individual characters from the file. The `while` loop continues until there are no more characters to read. Finally, the `close()` function is used to close the file when we are finished reading it.

It's important to note that the `open()` function requires the name of the text file as an argument. This file must be located in the root directory of the SD card, and the file name must be in 8.3 format (i.e. 8 characters for the file name and 3 characters for the extension).

## See Also

- [SD Library Reference](https://www.arduino.cc/en/Reference/SD)
- [Arduino SD Card Tutorial by Maker Pro](https://maker.pro/arduino/tutorial/how-to-interface-sd-card-with-arduino)
- [How to Read and Write Files on an SD Card with an Arduino by Circuit Basics](https://www.circuitbasics.com/how-to-write-to-an-sd-card-with-an-arduino/)