---
title:                "Arduino recipe: Creating a temporary file"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Why 

Are you looking for a way to store temporary data on your Arduino board? Perhaps you're building a project where you need to save some values or variables temporarily. Well, creating a temporary file may just be the solution you're looking for! By creating a temporary file, you can easily store and retrieve data on your board without permanently taking up space on your memory.

## How To

Creating a temporary file on your Arduino board is a simple process. Just follow these steps:

1. First, you will need to include the SPIFFS library in your code. This can be done by writing ```#include <SPIFFS.h>``` at the top of your sketch. 

2. Next, you will need to begin the SPIFFS file system by adding ```SPIFFS.begin()``` in the ```setup()``` function of your code.

3. Now, you can create a temporary file using the ```open``` function and assign it a desired name. For example, ```File tempFile = SPIFFS.open("/temp.txt", "w");```. 

4. You can then write data to your temporary file using the ```write``` function or read data from it using the ```read``` function.

5. Once you are done using the temporary file, make sure to close it by using the ```close``` function. This will ensure that the data is saved and the file is removed from the memory.

Here's a simple example of how to create a temporary file and write data to it:

```Arduino
#include <SPIFFS.h>

void setup() {
  Serial.begin(9600);
  SPIFFS.begin();
  
  //create temporary file named "temp.txt"
  File tempFile = SPIFFS.open("/temp.txt", "w");
  
  //write "Hello World!" to the file
  tempFile.write("Hello World!");
  
  //close the file
  tempFile.close();
}

void loop() {
  //do nothing
}
```

After uploading this code to your Arduino board, you can go to the SPIFFS file system and check if the temporary file has been created.

## Deep Dive

When you create a temporary file, it is stored in a portion of the memory called the SPIFFS file system. The SPIFFS library allows you to easily access and manipulate this file system. You can create, read, write, and delete files using various functions provided by the library.

When creating a temporary file, you can choose to give it any name you want, as long as it follows the correct file naming rules. Additionally, you can choose to open the file in different modes such as "read", "write", or "append", depending on your needs.

It is important to note that temporary files are not limited to only storing text data. You can also store binary data such as images, audio files, and more.

## See Also

If you'd like to learn more about the SPIFFS library and how to use temporary files, here are some helpful resources for you to check out:

- [Arduino SPIFFS Library Documentation](https://arduino-esp8266.readthedocs.io/en/latest/filesystem.html)
- [Tutorial: How to Use SPIFFS on the ESP8266](https://randomnerdtutorials.com/esp8266-nodemcu-vs-code-platformio-spiffs/)
- [YouTube Video: How to Create and Read a File on SPIFFS using ESP8266](https://www.youtube.com/watch?v=qE6WnfbGLE4)

Now that you know how to create and use temporary files on your Arduino board, go ahead and try it out in your next project! It can be a useful tool for storing and retrieving data temporarily without taking up memory space permanently. Happy coding!