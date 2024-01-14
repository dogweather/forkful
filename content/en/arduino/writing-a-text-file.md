---
title:    "Arduino recipe: Writing a text file"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

# Why

Whether you're a beginner or an experienced programmer, learning how to write a text file with Arduino can be a useful skill to have. Text files allow you to store and retrieve data, making your projects more versatile and dynamic. They also provide a simple way to communicate with other devices or systems.

## How To
To write a text file with Arduino, you'll need to follow these steps:

1. Start by connecting your Arduino board to your computer.

2. Open the Arduino IDE and create a new sketch.

3. Next, we'll need to include the SD card library. This library allows us to read and write files on an SD card. In the **setup()** function, add the following code:

```Arduino
#include <SD.h>
```

4. Initialize the SD card by calling the **begin()** function. This function takes in the pin number for the SD card's chip select (CS) pin. In most cases, this will be pin 53.

```Arduino
SD.begin(53);
```

5. Now, we can open a file to write data to. The **open()** function takes in three parameters: the file name, the mode to open the file in, and the SD card chip select pin. The mode we'll be using is **FILE_WRITE**, which allows us to write data to the file. For example, to open a file named "data.txt" we would use the following code:

```Arduino
File myFile = SD.open("data.txt", FILE_WRITE, 53);
```

6. Once the file is open, we can write data to it using the **println()** function. This function takes in the data to be written as a parameter. For example, let's write the string "Hello World" to our file:

```Arduino
myFile.println("Hello World");
```

7. Don't forget to close the file once you're done writing data. This is important as it ensures that all data is properly saved. To do this, simply call the **close()** function on the file object.

```Arduino
myFile.close();
```

8. Congratulations, you have successfully written data to a text file with Arduino! You can now use the same steps to write other types of data, such as sensor readings or user inputs, to a file.

## Deep Dive
When writing a text file with Arduino, it's important to keep in mind that the SD card has limited space. Depending on the size of the SD card, you may need to be selective about the data you write.

It's also worth noting that the **open()** function has additional modes that can be used besides **FILE_WRITE**. For example, **FILE_APPEND** allows you to append data to an existing file without overwriting it, while **FILE_READ** allows you to read data from a file.

In addition, you can also create directories within the SD card using the **mkdir()** function. This can help organize your files and make it easier to retrieve specific data.

## See Also
To learn more about writing text files with Arduino, check out these useful resources:

- [Arduino SD Library Reference](https://www.arduino.cc/en/reference/SD)
- [Instructables: How to Write SD Card Program Using Arduino](https://www.instructables.com/id/ARDUINO-How-to-write-a-text-file-on-a-SD-card/)
- [Hackster: How to Save Data to a CSV File on an SD Card With Arduino](https://www.hackster.io/RonaldoOvermeire/how-to-save-data-to-a-csv-file-on-an-sd-card-with-arduino-1487d7)