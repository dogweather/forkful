---
title:                "Creating a temporary file"
html_title:           "Arduino recipe: Creating a temporary file"
simple_title:         "Creating a temporary file"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## What & Why?
Creating a temporary file is a common practice in programming that involves temporarily storing data in a file for later use. This is often done when working with large amounts of data or when the precise location and name of a file are not known during development. By creating a temporary file, programmers can easily manipulate and retrieve data without having to constantly write and rewrite new files.

## How to:
To create a temporary file in Arduino, you can use the ```File::createTemp``` function. This function takes in two arguments: the desired name of the temporary file and the size of the file. Here's an example of creating a 100 byte temporary file named "temp.txt":
```
#include <SPI.h> //include SPI library

void setup() {
  Serial.begin(9600); //start serial communication
  File tempFile = File::createTemp("temp.txt", 100); //create temporary file
  Serial.println("Temporary file created!");
  tempFile.close(); //close file
}

void loop() {
  //do something
}
```
Running this code will output "Temporary file created!" to the serial monitor. The file will remain on the Arduino's SD card until it is manually deleted.

## Deep Dive:
Creating temporary files has been a common practice in programming since the early days of file storage. Before the advent of modern file systems, creating temporary files was necessary in order to manipulate and save data without having to constantly overwrite existing files. Today, temporary files are often used in situations where the file name and location are not known in advance, such as when working with user-generated data.

An alternative to creating a temporary file is to use temporary variables in the program's memory. However, this can lead to memory limitations and is not always feasible when working with large amounts of data.

To implement the ```File::createTemp``` function, Arduino uses the fopen and mkstemp functions from the C standard library. These functions create a file with a randomly generated name to avoid naming conflicts. The file is then opened and can be manipulated like any other file.

## See Also:
- [Arduino's File class documentation](https://www.arduino.cc/en/Reference/File)
- [C fopen function documentation](https://www.cplusplus.com/reference/cstdio/fopen/)
- [C mkstemp function documentation](https://www.cplusplus.com/reference/cstdio/mkstemp/)