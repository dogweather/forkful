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

## What & Why?

Reading a text file means accessing and extracting data from a file that contains text. Programmers often do this in their code when they need to retrieve specific information from a text file, such as configuration settings or user input.

## How to:
To read a text file in Arduino, you will need to use the SD library. This library enables you to communicate with an SD card to read and write data. To start, you will need to include the SD library in your code:

```Arduino
#include <SD.h>
```

Next, you will need to initialize an SD card. This is done by using the `SD.begin()` function, which takes in the pin connected to your SD card's chip select (CS) pin. This is usually pin 4 on most Arduino boards.

```Arduino
if (!SD.begin(4)) {
  // SD card initialization failed
  return;
}
```

Once the SD card is initialized, you can open the text file using the `SD.open()` function. This function takes in the name of the file you want to open and the file mode, which in this case is `FILE_READ` to indicate that we want to read the file.

```Arduino
File file = SD.open("data.txt", FILE_READ);
```

You can then use the `file.read()` function to read a single character from the file and store it in a variable. This function returns -1 if no character is available, which we can use as a condition in a while loop to iterate through the entire file and print its contents.

```Arduino
while (file.available()) {
  char character = file.read();
  Serial.print(character);
}
```

The sample output for this code would be the contents of the text file printed to the serial monitor.

## Deep Dive:
Reading text files has been a common practice in computer programming for decades. It allows for the storage and retrieval of larger amounts of data in a more organized and easily readable format. Prior to the development of SD cards and the SD library, programmers had to use different techniques, such as reading data from a serial port or using external memory chips.

While using the SD library is the most common way to read text files in Arduino, there are other alternatives such as the FileIO library or the SPI library. Each of these options has its own advantages and limitations, so it is important to choose the best one for your specific project.

In terms of implementation, it is worth noting that the `file.read()` function only reads a single character at a time. This means you may need to use additional functions, such as `file.seek()` and `file.seekEnd()` to move through the file and retrieve specific information.

## See Also:
- SD library reference: https://www.arduino.cc/en/Reference/SD
- FileIO library: https://github.com/greiman/SdFat
- SPI library: https://www.arduino.cc/en/Reference/SPI