---
title:    "Arduino recipe: Creating a temporary file"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/arduino/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Why

Creating temporary files may seem like a trivial task, but it can actually be very useful in certain situations. For example, if you are working on a project that requires writing and reading data from a file, creating a temporary file can serve as a temporary storage space for your data before it is permanently saved in another file. This allows for easier manipulation and management of data without risking the integrity of your original file.

## How To

Creating a temporary file in Arduino is a simple process. First, you need to include the avr/io.h library by adding the following line to your code:

```Arduino
#include <avr/io.h>
```

Next, you will need to declare a temporary file variable using the "FILE" data type and specify the directory where you want the temporary file to be created. For this example, we will create a temporary file on the SD card, so we will use the "SD.open()" function to specify the directory:

```Arduino
FILE tempFile = SD.open("temp.txt", FILE_WRITE);
```

Now, you can write data to your temporary file using the "fprintf()" function. In the following example, we will write the string "Hello World!" to our temporary file:

```Arduino
fprintf(tempFile, "Hello World!");
```

Once you are finished writing to the temporary file, you can close it using the "fclose()" function:

```Arduino
fclose(tempFile);
```

To read data from the temporary file, you can use the "fscanf()" function. For example, if you wanted to read the data written in the previous step, you could use the following code:

```Arduino
char data[13]; // create an array to store the data
fscanf(tempFile, "%s", data); // read data from the file and store it in the array
```

The code above will store "Hello World!" in the "data" array. You can now use and manipulate this data as needed.

## Deep Dive

Creating a temporary file involves several steps, including specifying the file directory, opening the file, writing data, and closing the file. However, it is important to note that the temporary file will only be created in the specified directory when the file is opened using the "SD.open()" function. If the directory does not exist, the file will not be created.

Also, it is good practice to delete the temporary file once it is no longer needed. This can be done using the "SD.remove()" function:

```Arduino
SD.remove("temp.txt"); // deletes the temporary file from the specified directory
```

Furthermore, if you want to create a temporary file on your computer instead of on an SD card, you can use the same steps but replace "SD." with "FILE." in the code.

## See Also

- [SD card library documentation - Arduino](https://www.arduino.cc/en/Reference/SD)
- [File handling in C - Tutorialspoint](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)
- [Temporary files in programming - GeeksforGeeks](https://www.geeksforgeeks.org/temporary-files-in-programming/)