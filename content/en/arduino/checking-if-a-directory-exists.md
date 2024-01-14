---
title:    "Arduino recipe: Checking if a directory exists"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Why
If you're working on an Arduino project that involves saving or accessing files, it's important to know whether a directory exists before trying to read or write from it. This will prevent errors and ensure that your code runs smoothly.

## How To
To check if a directory exists in your Arduino project, you can use the `SD.exists()` function from the SD card library. This function takes in a string parameter of the directory name and returns a boolean value, `true` if the directory exists and `false` if it doesn't.

```Arduino
#include <SPI.h>
#include <SD.h>

void setup() {
  // initialize SD card
  SD.begin(10);

  // check if directory named "data" exists
  if (SD.exists("data")) {
    Serial.println("Directory exists!");
  } else {
    Serial.println("Directory does not exist.");
  }
}

void loop() {

}
```
Output:
```
Directory exists!
```

If you want to check for a specific file within the directory, you can use the `File::exists()` function instead. This function takes in a String parameter of the file name and returns a boolean value.

```Arduino
#include <SPI.h>
#include <SD.h>

void setup() {
  // initialize SD card
  SD.begin(10);

  // check if file named "text.txt" exists in "data" directory
  if (File::exists("data/text.txt")) {
    Serial.println("File exists!");
  } else {
    Serial.println("File does not exist.");
  }
}

void loop() {

}
```
Output:
```
File exists!
```

## Deep Dive
Behind the scenes, the `SD.exists()` function uses the underlying `fexists()` function from the SD card library. This function takes in a `File` object as a parameter and uses the `FatStream::getFilename()` function to extract the file name and check for its existence.

It's important to note that the `SD.exists()` function will only work for directories and files within the root directory of the SD card. If you want to check for a subdirectory or nested file, you will need to use the `File::open()` function to create a `File` object and then use the `fexists()` function.

## See Also
- [SD Library Documentation](https://www.arduino.cc/en/Reference/SD)
- [FatLib Library Documentation](https://github.com/greiman/FatLib)