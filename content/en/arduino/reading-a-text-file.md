---
title:                "Arduino recipe: Reading a text file"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/reading-a-text-file.md"
---

{{< edit_this_page >}}

## Why

If you're an Arduino enthusiast or a beginner looking to learn more about programming this versatile microcontroller, you might have come across the term "reading a text file". Reading a text file allows you to store and retrieve data, making your projects more dynamic and interactive. In this blog post, we'll explore the process of reading a text file on an Arduino board.

## How To

Most programming languages have built-in functions or libraries to make reading text files a simple task. Thankfully, the Arduino programming language has a similar function called `Open()`. Let's take a simple example of how to use `Open()` to read a text file and print its contents on the serial monitor.

```Arduino
void setup() {
  Serial.begin(9600); // initialize serial monitor
  File myFile = SD.open("data.txt"); // open a text file named "data"
  while (myFile.available()) { // loop until end of file
    Serial.println(myFile.readStringUntil('\n')); // print line by line
  }
}

void loop() {
  // nothing to do here
}
```

To use the `Open()` function, you'll need to include the `SD` library in your code. This library allows for the use of a microSD card as a storage device for your Arduino. In this case, our text file `data.txt` is stored on the microSD card.

Now, let's see the output of this code on the serial monitor:

```
Hello, world!
This is a test text file.
It contains multiple lines of text.
```

As you can see, we have successfully read and printed the contents of our text file using the `Open()` function.

## Deep Dive

Now, let's delve deeper into the process of reading a text file on an Arduino board. First, it's important to understand the structure of a text file. A text file consists of a series of characters, including letters, numbers, and symbols, organized into lines. These lines are separated by special characters known as newline characters, which signify the end of a line.

When using the `Open()` function, we specify a file name and a mode. The mode can be either `READ` or `WRITE`, depending on whether you want to read or write to the file, respectively. In our example, we used the default `READ` mode.

After the `Open()` function is used, we can use another function called `available()` to check if there are still characters available to read. If so, we can use the `readStringUntil()` function to read the text until it reaches the newline character `'\n'`.

It's important to note that there are limitations to reading text files on an Arduino board. The board has limited memory space, so large text files may not be able to be fully read. Also, the size of the text file must be specified in the code for the `readStringUntil()` function to work properly.

## See Also

To further explore reading and writing text files on Arduino, check out these helpful resources:

- [Arduino Reference for SD Library](https://www.arduino.cc/en/Reference/SD) - Official documentation on the `SD` library and its functions.
- [Arduino SD Library Example](https://www.arduino.cc/en/Tutorial/Files) - A step-by-step tutorial on storing and retrieving data using the `SD` library.

Now that you have a basic understanding of reading text files on an Arduino board, you can start implementing this feature in your own projects and make them more dynamic. Happy coding!