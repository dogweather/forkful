---
title:                "Arduino recipe: Creating a temporary file"
simple_title:         "Creating a temporary file"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to store some data temporarily in your Arduino project? Maybe you need to keep track of sensor readings or save user input. In cases like these, creating a temporary file can be extremely useful.

## How To

To create a temporary file in Arduino, you will need to use the File class from the SD library. First, you will need to initialize an SD card object using the `begin()` function. Then, you can create a new file using the `open()` function, passing in a file name and the mode "a+" to append data to the file.

```
ArduinoSD.begin(4); // initialize SD card object
File tempFile = SD.open("temp.txt", FILE_APPEND); // create and open file
```

Once you have your temporary file, you can write data to it using the `println()` function, just like writing to the Serial monitor. Don't forget to close the file when you're done writing to it.

```
tempFile.println("Sensor reading: 50"); // write to file
tempFile.close(); // close file
```

To read data from the file, you can use the `read()` and `peek()` functions. Check out the Arduino SD library reference for more information on how to work with files.

## Deep Dive

Creating temporary files can also be useful for storing settings or configurations in Arduino projects. You can write data to the file and access it later to change the behavior of your project without having to re-upload code.

It's important to note that temporary files are not permanent storage and will be deleted when the program restarts. If you need to permanently save data, you can use EEPROM or external memory solutions.

## See Also

- [Arduino SD library reference](https://www.arduino.cc/en/Reference/SD)
- [Overview of SD memory cards and SPI interface](https://www.arduino.cc/en/Tutorial/Foundations/SD_Cards)
- [Storing and reading settings using temporary files](https://www.arduino.cc/en/Tutorial/ReadWriteEeprom)