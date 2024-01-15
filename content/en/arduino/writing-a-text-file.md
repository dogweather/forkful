---
title:                "Writing a text file"
html_title:           "Arduino recipe: Writing a text file"
simple_title:         "Writing a text file"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why

Writing a text file is a common task in programming, especially when using a microcontroller like Arduino. It allows us to store data and information that can be accessed and used later on.

## How To

```Arduino
#include <SPI.h>
#include <SD.h>

File myFile;

void setup() {
  Serial.begin(9600);
  SD.begin(4); // set pin for SD Card
  myFile = SD.open("myTextFile.txt", FILE_WRITE); // open file for writing
  myFile.println("Hello World!"); // write data to file
  myFile.close(); // close file
}

void loop() {
  // do something else here
}
```

In the above example, we first include the necessary library for using an SD card and create a file object. Then, in the `setup` function, we begin the SD card and open the file using the `open` function. We can then use the `println` function to write our desired data to the file. Finally, we close the file using the `close` function. The file will be created in the SD card with the name "myTextFile.txt". 

## Deep Dive

In order to understand the process of writing a text file in Arduino, it's important to understand a few key concepts.

### Library Inclusion

In order to use an SD card with Arduino, we need to include the `SPI.h` and `SD.h` libraries. The `SPI.h` library allows communication with devices using the SPI protocol, while the `SD.h` library provides functions for accessing the SD card.

### Setting up the SD Card

Before we can use the SD card, we need to set it up. This is done using the `SD.begin()` function, which takes the pin number where the SD card is connected as an argument. In our example, we have used pin number 4.

### File Object Creation

To interact with a file in Arduino, we need to create a `File` object. This object will be used to open, read, write, and close the file.

### Opening the File

To open a file, we use the `open()` function of the `File` object. It takes two arguments - the name of the file and the mode in which the file is to be opened. In our example, we have used `"myTextFile.txt"` as the file name and `FILE_WRITE` as the mode, which means we are opening the file for writing.

### Writing to the File

To write to a file, we can use the `print()` or `println()` function of the `File` object. It works just like the `Serial` object, where we can pass in any data type and it will be converted to a string and written to the file. In our example, we have used the `println()` function to automatically add a new line after the data.

### Closing the File

It is important to close the file once we are done writing to it using the `close()` function. This ensures that all data is written to the file and the file is properly closed.

## See Also

- [Arduino SD Library Documentation](https://www.arduino.cc/en/Reference/SD)
- [Arduino SPI Library Documentation](https://www.arduino.cc/en/Reference/SPI)
- [Writing files to an SD card with Arduino](https://www.instructables.com/Writing-files-to-an-SD-card-with-Arduino/)