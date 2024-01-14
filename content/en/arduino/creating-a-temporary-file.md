---
title:    "Arduino recipe: Creating a temporary file"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Why

If you're an Arduino enthusiast, you may have come across the need to create a temporary file in your programming. This could be for a variety of reasons such as storing temporary data, organizing files, or creating a backup. Whatever your reason may be, understanding how to create a temporary file in your Arduino code can be a useful skill to have. In this blog post, we'll explore the reasons why you may need to create a temporary file and how to do it in an Arduino sketch.

## How To

To create a temporary file in Arduino, we will be using the `File` class and its `createTempFile()` function. This function takes two arguments: the prefix and the suffix for the file name. The prefix is the starting text of the file name while the suffix is the file extension. Let's take a look at an example code:

```Arduino
#include <SD.h> // Include the SD library

File tempFile; // Create a File object

void setup() {
  // Initialize SD card
  SD.begin(4);
  // Create a new temporary file with the prefix "temp" and suffix ".txt"
  tempFile = SD.open("temp", FILE_WRITE);
  // Write data to the file
  tempFile.println("This is a temporary file");
}

void loop() {
  // Do something with the temporary file
}

```

In the above code, we first include the `SD` library, which is necessary for working with files on an SD card. We then create a `File` object named `tempFile`. In the `setup()` function, we initialize the SD card and use the `open()` function to create a new temporary file with the prefix "temp" and suffix ".txt". We then use the `println()` function to write some data to the file. In the `loop()` function, you can perform any necessary operations on the temporary file.

When you run the above code, it will create a temporary file called "temp.txt" on your SD card which contains the line "This is a temporary file". You can modify the prefix and suffix arguments to suit your needs and create different types of temporary files.

## Deep Dive

The `createTempFile()` function in Arduino uses the standard C function `tempnam()` to generate a unique filename. This function creates a file in the temporary directory (usually `/tmp`) with a unique name and returns the path to that file. In Arduino, this file is then created on the SD card using the specified prefix and suffix.

It's important to note that these temporary files are not automatically deleted after use. It is the responsibility of the programmer to delete these files when they are no longer needed. This can be done using the `File` class's `remove()` function. Additionally, if a temporary file is not closed properly, it may corrupt data on the SD card.

## See Also

If you're interested in learning more about working with files and the SD card on Arduino, check out these useful resources:

- [Arduino - File](https://www.arduino.cc/reference/en/libraries/sd/file/)
- [Writing Data to Files on an SD Card](https://learn.adafruit.com/adafruit-guide-excellent-sd/how-to-create-file-system)
- [SD Library Example Sketches](https://www.arduino.cc/en/Reference/SD)