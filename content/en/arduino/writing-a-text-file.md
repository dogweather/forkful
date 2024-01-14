---
title:                "Arduino recipe: Writing a text file"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why
If you're an Arduino enthusiast, you may have come across the need to write a text file using your board. This can serve a variety of purposes such as storing sensor data, creating a log, or even just printing a message on a display. Whatever the reason may be, learning how to write a text file using Arduino can open up a whole new world of possibilities for your projects.

## How To

To write a text file using Arduino, we will be using the `File` library. Firstly, make sure to include the library by adding `#include <File.h>` at the top of your code. Next, initialize an object of the `File` class by calling `File myFile = File("filename.txt", FILE_WRITE);` where "filename.txt" is the name of your text file and FILE_WRITE ensures that we can write to the file.

Next, we can use the `print()` or `println()` functions to write text to our file. For example, `myFile.println("Hello World!");` will write the text "Hello World!" to our file. To save the changes, we need to close the file by calling `myFile.close();`.

Now, let's see an example of writing sensor data to a text file:

```Arduino
#include <File.h>

void setup() {

  // initialize serial communication
  Serial.begin(9600);

  // open file in write mode
  File myFile = File("sensor_data.txt", FILE_WRITE);
  
  // read temperature sensor value
  float temp = analogRead(A0) * 0.48828125; // convert from ADC value to Celsius
  
  // write temperature to file
  myFile.print("Current temperature: ");
  myFile.print(temp);
  myFile.println(" °C");

  // close file
  myFile.close();

}

void loop() {
  // do nothing
}
```

After running this code, you should see a new text file named "sensor_data.txt" containing the current temperature in Celsius. You can modify the code to write any other type of data or message to your text file.

## Deep Dive

The `print()` and `println()` functions can be used to write not just text, but also variables and other data types to your file. You can also use the `write()` function to write individual characters.

By default, the `File` library will create a new text file if it doesn't already exist. However, you can also use the `FILE_APPEND` flag when initializing your `File` object to append new data to an existing file instead of overwriting it.

It's important to note that the `File` library has a limited amount of memory for storing data. Therefore, it's recommended to use this method for smaller amounts of data, such as sensor readings, and not for large text files.

## See Also

- [Arduino File Class Reference](https://www.arduino.cc/reference/en/libraries/file/)
- [Writing to Files on Arduino](https://www.arduino.cc/en/Tutorial/WriteToFile)
- [How to Write Text Files](https://forum.arduino.cc/index.php?topic=421610.0)