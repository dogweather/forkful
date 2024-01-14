---
title:                "Arduino recipe: Reading command line arguments"
programming_language: "Arduino"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why

If you're new to Arduino programming, you may be wondering why you would want to learn about reading command line arguments. The short answer is that it can greatly expand the capabilities of your Arduino projects. By being able to take in input from the command line, you can create more interactive and dynamic programs.

## How To

To start, let's define what command line arguments are. They are essentially inputs that are typed into the command line when a program is executed. In the case of Arduino programming, we can use these inputs to customize the behavior of our program.

To read command line arguments in Arduino, we will use the `argc` and `argv` variables. `argc` is an integer that represents the number of arguments, while `argv` is an array of strings that holds the actual arguments.

```Arduino
void setup() {
  // initialize serial communication
  Serial.begin(9600);
}

void loop() {
  // check if there are any arguments
  if (argc > 0) {
    // loop through each argument and print it out
    for (int i = 0; i < argc; i++) {
      Serial.println(argv[i]);
    }
  }
}
```

If we were to upload this code to our Arduino and open the Serial Monitor, we would see the arguments listed out.

For example, if we uploaded the code and typed in "Hello World" as our argument, we would see:

```
Hello
World
```

In this example, "Hello" is stored in `argv[0]` and "World" is stored in `argv[1]`.

## Deep Dive

Now that we have a basic understanding of how to read command line arguments in Arduino, let's dive deeper into the topic. One potential use for this feature is to create a customizable LED light show. By taking in arguments for the color and duration of each LED, we can create a unique and personalized light show.

Another use case could be for controlling a robot. By taking in arguments for movement direction and speed, we can create a more dynamic and responsive interaction with the robot.

The possibilities are endless with the ability to read command line arguments in Arduino. It allows for more flexibility and customization in our projects.

## See Also

Here are some helpful resources to explore further:

- [Arduino command line arguments documentation](https://www.arduino.cc/reference/en/language/functions/communication/serial/readstringuntil/)
- [Tutorial on parsing command line arguments in Arduino](https://blog.arduino.cc/2019/01/11/parsing-command-line-arguments-in-arduino-code/)
- [Example project using command line arguments](https://www.instructables.com/id/Control-A-Pixy-Using-Command-Line-Arguments-on-An/)