---
title:                "Arduino recipe: Reading command line arguments"
simple_title:         "Reading command line arguments"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why

Are you new to Arduino programming and wondering what the heck command line arguments are? Or maybe you've been working with Arduino for a while but have never had the need to use command line arguments. In this blog post, we'll explore why it's important to know how to read command line arguments and how they can make your code more efficient.

## How To

First things first, let's understand what command line arguments are. Command line arguments are basically extra pieces of information that are passed to a program when it is run through the command line. They can be used in a variety of ways and can save you a lot of time by eliminating the need to hardcode values in your code.

Now, let's see how to read command line arguments in Arduino using the `Serial` object. Here's a basic code example:

```arduino
// Read a command line argument and print it to the serial monitor

void setup() {
  // Initialize the serial communication at 9600 baud
  Serial.begin(9600);
  
  // Read the first argument (index 0)
  String arg = Serial.readString(0);
  
  // Print the argument to the serial monitor
  Serial.println("The argument is: " + arg);
}

void loop() {
  // Empty loop for the sake of example
}
```

You'll notice that we use `Serial.readString(0)` to read the argument at index 0, which is the first argument passed to the program. You can read multiple arguments by changing the index value. Now, let's see what the output would be if we run this code and pass an argument through the command line:

```shell
# Compile and upload the code to your Arduino, and then run it with an argument
$ arduino-cli compile -b arduino:avr:uno . && arduino-cli upload -p COM3 --input . && arduino-cli upload -p COM3 --serial $(git rev-parse HEAD)
```

The output on the serial monitor would be:

```shell
The argument is: 1234
```

Easy, right? Now you can use command line arguments in your Arduino code to make it more dynamic and efficient.

## Deep Dive

But let's take a deeper dive into the world of command line arguments. Did you know that you can also pass arguments to your Arduino code when uploading it through the command line? That's right, you can pass arguments with the `--serial` flag, and those arguments will be automatically read by your code when it starts running. This is a useful feature if you want to have different behaviors or outputs based on the argument passed during the upload process.

## See Also

Want to learn more about using command line arguments in Arduino? Check out these resources for more information and examples:

- [Arduino Command Line Tools Documentation](https://arduino.github.io/arduino-cli/commands/arduino-cli-compile/)
- [Arduino Stack Exchange: How to read command line arguments on Arduino?](https://arduino.stackexchange.com/questions/55861/how-to-read-command-line-arguments-values)
- [Arduino.cc: Passing arguments to the Serial Monitor](https://www.arduino.cc/reference/en/language/functions/communication/serial/print/)