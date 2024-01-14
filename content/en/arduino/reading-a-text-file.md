---
title:    "Arduino recipe: Reading a text file"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Why

Are you an Arduino enthusiast looking to expand your programming skills? Or perhaps you're a beginner wanting to learn more about reading and manipulating data. Either way, understanding how to read a text file using Arduino can be a useful skill to have in your programming arsenal. It will allow you to import and use external data in your projects, giving you more versatility and control.

## How To

To read a text file using Arduino, we will be using the `SD` library. This library allows us to interface with an SD card and access its data. Here's a simple example of how to read and print the contents of a text file using the `SD` library:

```
#include <SPI.h>
#include <SD.h>

File myFile;

void setup() {
  Serial.begin(9600);

  if (!SD.begin(10)) {
    Serial.println("SD card initialization failed!");
    return;
  }

  myFile = SD.open("myFile.txt");

  if (myFile) {
    Serial.println("File opened successfully!");
    while (myFile.available()) {
      Serial.write(myFile.read());
    }
  } else {
    Serial.println("Error opening file!");
  }
}

void loop() {
  // Nothing to do here
}
```

In the above code, we first include the `SPI` and `SD` libraries, which are necessary for communicating with an SD card. Next, we initialize the serial communication and check if the SD card was successfully initialized. If not, the program will exit.

We then use `SD.open()` to open the text file we want to read. The file must be placed in the root directory of the SD card. If the file is opened successfully, we use a `while` loop to read and print each character in the file using `Serial.write()`. Finally, we close the file and go back to the `loop` function.

When we upload and run this code on an Arduino connected to an SD card, the contents of the text file will be printed in the serial monitor.

## Deep Dive

Now, let's take a deeper look at the `SD.open()` function. This function takes in the name of the file you want to open as its parameter. However, it can also take in a second parameter, which specifies the mode in which you want to open the file.

The default mode is `FILE_READ`, which allows you to only read from the file. However, you can also use `FILE_WRITE` to write to the file or `FILE_APPEND` to add new data at the end of the file. You can also use other modes like `FILE_DELETE` or `FILE_READWRITE` for more advanced operations.

It's also important to mention that the `SD.open()` function returns a `File` object, which you can use to perform other file operations like `close()`, `seek()`, or `available()`.

## See Also

- [SD library reference](https://www.arduino.cc/en/Reference/SD)
- [SD card tutorial by Arduino](https://www.arduino.cc/en/Tutorial/SdCard)
- [Arduino File documentation](https://www.arduino.cc/en/Reference/File)