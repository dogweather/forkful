---
title:    "Arduino recipe: Writing to standard error"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/arduino/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why

In the world of Arduino programming, there may come a time when you need to output information to a different location other than the standard output. This is where writing to standard error comes in. By using this technique, you can easily debug your code and catch any errors that may arise during execution.

## How To

To write to standard error, you can use the `Serial.print()` function with the `Serial` object. Here's an example code:

```Arduino
int x = 10;

Serial.print("The value of x is: ");
Serial.print(x, DEC);
Serial.println();
Serial.print("This is an error message.");
```

The above code will output the value of `x` and a custom error message to the serial monitor. Here's the sample output:

The value of x is: 10
This is an error message.

As you can see, the error message is displayed in a different line and stands out from the rest of the output. This makes debugging much easier as you can easily spot any errors that occur during execution.

## Deep Dive

Writing to standard error is useful when you want to output any error messages or important information during runtime. It can be particularly helpful when working with sensors or other external devices, as it allows you to monitor their values and catch any errors that may occur.

To further customize the output, you can use the `Serial.println()` function instead of `Serial.print()`. This will add a new line at the end of the output, making it easier to read and distinguish between different messages.

Another tip is to use serial communication for writing to standard error instead of using `Serial.print()` directly in your main code. This way, you can easily turn off the error messages in your final code by simply commenting out the serial communication code.

## See Also

For more information on writing to standard error and other useful Arduino techniques, check out the following resources:

- [Arduino Serial Communication documentation](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Instructables tutorial on using Serial communication on Arduino](https://www.instructables.com/Lesson-5-Connecting-a-Pushbutton-to-Arduino-Breadb/)
- [Maker Pro guide to Arduino troubleshooting and debugging](https://maker.pro/arduino/projects/troubleshooting-and-debugging-your-arduino-project)

Remember, utilizing techniques like writing to standard error can greatly improve your debugging process and make your code more efficient. Happy coding!