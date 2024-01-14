---
title:    "Arduino recipe: Writing a text file"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/arduino/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Why 
As Arduino programmers, we often use text files to store and retrieve data in our projects. Whether it's saving sensor readings or creating a simple user interface, writing a text file can be a useful and efficient way to store information. In this blog post, we will explore how to write a text file using Arduino programming. 

## How To 
To write a text file using Arduino, you will need to use the SD (Secure Digital) library. This library allows us to access a microSD card and read and write files on it. Here is a simple example of how to write a text file using Arduino:

```Arduino
#include <SPI.h>
#include <SD.h>

//Initialize microSD card
if (!SD.begin(4)) {
	Serial.println("Initialization failed!");
	return;
}
//Create and open a new text file
File myFile = SD.open("myFile.txt", FILE_WRITE);
//Write data to the file
myFile.println("This is a text file written using Arduino!");
//Close the file
myFile.close();
```

The `FILE_WRITE` mode in the `open()` function allows us to write to the file, while the `println()` function writes a line of text to the file. In this example, the file was named "myFile.txt", but you can choose any name you want.

To check if the text file was successfully written, you can use a microSD card reader and open the file on your computer. You should see the text we wrote in the file "This is a text file written using Arduino!".

## Deep Dive 
Now, let's dive deeper into the process of writing a text file with Arduino. It's important to note that the SD library has a limitation when it comes to file sizes - the maximum file size is 2GB. 

When creating a new file, we use the `open()` function as shown in the example above. This function takes in two parameters - the file name and the mode. The mode argument defines how we want to interact with the file - whether we want to read, write, or both. Some of the common mode options are `FILE_READ`, `FILE_WRITE`, and `FILE_APPEND` which allows us to append new data to the end of the file without overwriting the existing data.

After creating the file, we can use the `write()` function to write individual characters or `println()` to write full lines of text. Once we are done writing, we need to close the file using the `close()` function to ensure all data is saved to the file before we access it again.

## See Also 
For more information on writing text files using Arduino, you can check out these resources: 
- [Arduino - SD Library](https://www.arduino.cc/en/reference/SD)
- [Tutorial: How to Write a Text File to an SD Card using Arduino](https://randomnerdtutorials.com/guide-for-writing-a-text-file-to-an-sd-card-using-arduino/) 
- [How to Save Data to an SD Card using Arduino](https://www.makerguides.com/arduino-sd-card-write-text-file-example/)

Now that you know how to write a text file using Arduino, you can use this knowledge in your own projects. Happy coding!