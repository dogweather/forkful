---
title:    "Arduino recipe: Reading a text file"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why
Text files are a common way to store data, and being able to read them is a useful skill for any programmer. In regards to Arduino, being able to read a text file opens up possibilities for storing and accessing information from external sources, making your projects more dynamic and versatile.

## How To
Reading a text file in Arduino is a fairly straightforward process. First, you will need to have your Arduino connected to your computer and have the Arduino IDE installed. Then, follow these steps:

1. Create a new sketch in the Arduino IDE.
2. Open the `File` menu and select `Examples`.
3. Navigate to `SD` and select `cardInfo`.
4. This example code will read a text file called `TEST.TXT` from the SD card inserted in your Arduino.
5. Upload the code to your Arduino and open the serial monitor to see the output.

```
// include the SD library
#include <SD.h>

// set pin for the SD card
const int chipSelect = 4;

void setup() {
  // initialize serial communication
  Serial.begin(9600);
  // see if the card is present and can be initialized
  if (!SD.begin(chipSelect)) {
    //if the card is not present, return
    Serial.println("Card failed, or not present");
    return;
  }
  
  // open the file
  File dataFile = SD.open("TEST.TXT");
  
  // if the file is available, read it
  if (dataFile) {
    while (dataFile.available()) {
      // read one line at a time and output it to the serial monitor
      Serial.println(dataFile.readStringUntil('\n'));
    }
    // close the file
    dataFile.close();
  }
}

void loop() {
  // nothing here
}
```

In this example, the code uses the `File` object to open and read the contents of the `TEST.TXT` file. The `readStringUntil()` function allows us to read until a specific character, in this case, the newline character `'\n'`. This allows us to read the file line by line rather than all at once.

The output in the serial monitor will display the contents of the text file line by line, as shown in the example code above.

## Deep Dive
If you want to dive deeper into reading text files in Arduino, there are a few things to keep in mind. Firstly, make sure the text file is saved in the correct format - UTF-8 without a BOM (Byte Order Mark). This is the format that Arduino can read and process.

You can also use the `read()` function to read individual characters from the file, or the `parseInt()` function to read integer values from a specific position in the file.

It is also important to properly close the file after you have finished reading it, using the `close()` function. This ensures that the file is not corrupted and can be accessed again in the future.

## See Also
- [Arduino SD Library Documentation](https://www.arduino.cc/en/Reference/SD)
- [Reading and Writing Files on SD Cards with Arduino](https://www.circuitbasics.com/arduino-write-to-file/)
- [Reading and Writing from Text Files in Arduino Projects](https://www.dummies.com/programming/electronics/arduino/reading-and-writing-to-and-from-text-files-in-an-arduino-project/)