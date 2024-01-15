---
title:                "Reading a text file"
html_title:           "Arduino recipe: Reading a text file"
simple_title:         "Reading a text file"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why
Reading a text file is a crucial task in programming, especially when working with data stored in a file. By learning how to read a text file in Arduino, you can access important information and use it in your code to enhance the functionality of your project.

## How To
To read a text file in Arduino, follow these simple steps:

1. First, create a text file with the desired data. You can use any text editor, such as Notepad or Sublime Text, to create the file.
2. Save the text file with a .txt extension and transfer it to your Arduino board using a USB connection.
3. Next, open the Arduino IDE and create a new sketch.
4. Declare a variable to store the data from the text file, for example, `String data;`.
5. Use the `SD` library to initialize the SD card module and `SD.begin()` to open the connection between the Arduino and the SD card.
6. Use the `SD.open()` function to open the text file, passing the file name as the parameter.
7. Use an `if` statement to check if the file is open, and if it is, use the `readString()` function to read the data from the file and store it in the `data` variable.
8. Use the `SD.close()` function to close the file once you have finished reading it.
9. You can now use the data from the text file in your code, for example, by printing it to the serial monitor or using it in a calculation.

Take a look at the example code below to see how it all comes together:

```Arduino
#include <SPI.h>
#include <SD.h>

String data;

void setup() {
  Serial.begin(9600);
  while (!Serial) {
    ; // wait for serial port to connect.
  }
  if (!SD.begin(4)) {  // change according to your setup
    Serial.println("SD card initialization failed!");
    while (true) {}
  }
  
  File myFile = SD.open("data.txt");

  if (myFile) {
    data = myFile.readString();
    Serial.println(data);
    myFile.close();
  } else {
    Serial.println("error opening data.txt");
  }
}

void loop() {}
```

Output:
```
Hello World!
```

## Deep Dive
The `SD` library in Arduino provides convenient functions to work with SD cards. When using the `readString()` function, the data is read from the file until a newline character is encountered. You can also use the `read()` function to read a single character at a time.

It is important to note that the text file must be saved in the root directory of the SD card for the `SD.open()` function to work. If you want to save the text file in a specific location, you can use the `File` class to specify the path.

## See Also
- [SD library reference](https://www.arduino.cc/en/Reference/SD)
- [Arduino File class](https://www.arduino.cc/en/Reference/SDFile)
- [SD Card Module tutorial](https://create.arduino.cc/projecthub/ejsingh/sd-card-module-tutorial-how-to-add-more-data-storage-to-arduino-451098)