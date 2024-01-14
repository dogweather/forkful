---
title:    "Arduino recipe: Writing to standard error"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Why
If you're familiar with Arduino programming, you may have come across the `Serial.print()` function for debugging and displaying results. But have you ever wondered why there's also a `Serial.println()`? In this blog post, we'll dive into the world of writing to standard error in Arduino programming.

## How To
To start off, let's look at a simple code snippet using `Serial.println()`:

```Arduino
int num1 = 10;
int num2 = 5;

int result = num1 + num2;

Serial.println(result);
```

The above code will display the value of `result` on the serial monitor as 15. But what if we want to add more details to our output? This is where `Serial.print()` comes in. Let's modify our code to use `Serial.print()` instead:

```Arduino
int num1 = 10;
int num2 = 5;

int result = num1 + num2;

Serial.print("The result is: ");
Serial.println(result);
```

Notice how we used `Serial.print()` to display the text "The result is: " and then used `Serial.println()` to display the value of `result` on the same line. This can be particularly useful for debugging and providing more information about your program's output.

But what if we want to display our output on a separate line? This is where standard error comes in. Our modified code would look like this:

```Arduino
int num1 = 10;
int num2 = 5;

int result = num1 + num2;

Serial.print("The result is: ");
Serial.println(result, STDERR);
```

By adding the parameter `STDERR` in our `Serial.println()` function, we're indicating that we want the output to be displayed on standard error instead of the default standard output.

## Deep Dive
So, what exactly is standard error? In simple terms, it's a stream used for error messages or status updates in computer programming. In the context of Arduino programming, standard error is the stream used for displaying messages on the serial monitor.

By default, `Serial.println()` uses the standard output stream (STDOUT) to display output on the serial monitor, while `Serial.println()` uses the standard input stream (STDIN) for receiving input from the serial monitor.

Using standard error allows for more organization in displaying multiple outputs on the serial monitor. For example, we can use `Serial.print()` on standard output to display a header and then use `Serial.println()` on standard error to display the actual output, neatly separating the two on the serial monitor.

## See Also
Want to learn more about handling standard error in Arduino programming? Check out these helpful links:

- [Arduino Serial Reference](https://www.arduino.cc/reference/en/language/functions/communication/serial/)
- [Understanding Standard Streams in Programming](https://www.digitalocean.com/community/tutorials/understanding-standard-streams-in-linux)
- [5 Most Important Streams in Java](https://www.baeldung.com/java-io-streams)