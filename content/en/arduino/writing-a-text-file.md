---
title:                "Arduino recipe: Writing a text file"
simple_title:         "Writing a text file"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why
When working with Arduino, you may come across situations where you need to store data in a more permanent manner. Text files are a great way to achieve this as they allow you to store data that can be easily read and modified.

## How To
To write a text file in Arduino, you will need to use the `File` library. First, you will need to include the library by adding `#include <File.h>` at the top of your sketch. Then, you will need to create a `File` object using the `File` class by declaring it with a name of your choice, for example `myFile`. This file object will handle all the operations related to the text file.

Next, you will need to open the text file using the `open()` method, which takes in two parameters: the name of the file and the mode in which it should be opened. The modes available are `FILE_READ` for reading a file and `FILE_WRITE` for writing to a file. For example, you can use `myFile.open("data.txt", FILE_WRITE)` to open a file named "data.txt" for writing.

Once the file is opened, you can start writing to it using the `print()` or `println()` methods, which work similar to the `Serial` class. For example, `myFile.println("Hello, world!")` will write the string "Hello, world!" to the file on a new line.

After you have finished writing to the file, you will need to close it using the `close()` method. This will ensure that all the data is properly saved and the file is closed to allow other programs to access it.

Here is a complete example of writing to a text file:

```Arduino
#include <File.h>

File myFile;

void setup() {
  // Initialize serial communication
  Serial.begin(9600);

  // Open file for writing
  myFile.open("data.txt", FILE_WRITE);

  // Write data to file
  myFile.print("Temperature: ");
  myFile.println(25.5);
  myFile.print("Humidity: ");
  myFile.println(58);

  // Close file
  myFile.close();

  // Print message to confirm file was written
  Serial.println("Data written to file!");
}

void loop() {
  // Do nothing
}
```

The output of this code will be a new text file named "data.txt" with the following content:

```
Temperature: 25.5
Humidity: 58
```

## Deep Dive
When using the `open()` method, you can also specify additional parameters to customize how the file will be handled. For example, you can use `FILE_WRITE_APPEND` to open an existing file and write new data at the end instead of overwriting the existing data.

You can also use the `truncate()` method to clear the contents of a file before writing new data to it. This can be useful when you want to update the data in a file without creating a new file.

Additionally, you can use the `seek()` method to move the writing position to a specific point in the file. This is helpful when you want to write data at a specific location in the file instead of just appending it at the end.

Overall, using text files in Arduino gives you the flexibility to store and manage data in a more organized manner. With the various methods available, you can easily customize how you want to handle the file depending on your needs.

## See Also
- [Arduino File Library Reference](https://www.arduino.cc/en/Reference/File)
- [Arduino Serial Communication](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [C++ File Input/Output](https://www.cplusplus.com/doc/tutorial/files/)