---
title:    "Arduino recipe: Concatenating strings"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/arduino/concatenating-strings.md"
---

{{< edit_this_page >}}

## Why

Have you ever needed to combine multiple pieces of information into one string? Perhaps you want to create a more dynamic output or display a message with variable inputs. That's where string concatenation comes in! By combining strings, you can create more versatile and personalized outputs in your Arduino programs.

## How To

Coding string concatenation in Arduino is actually quite simple. First, you'll need to declare your strings using the `String` data type. For example, `String greeting = "Hello ";` and `String name = "John";` Then, you can combine them using the `+` symbol. For example, `String message = greeting + name;` will result in the string "Hello John".

Let's try a more practical example. Say you want to display the temperature on an LCD display along with the unit of measurement. You can use concatenation to achieve this. First, declare your variable for temperature using `int` data type, and set it to a value (e.g. `int temp = 25;`). Then, declare a string for the unit of measurement (e.g. `String unit = "°C";`). Finally, combine them using `+` symbol and display the output on the LCD screen. Here's the code:

```Arduino
#include <LiquidCrystal.h> //include library for LCD display
LiquidCrystal lcd(12, 11, 5, 4, 3, 2); //set LCD pins

int temp = 25; //declare temperature variable
String unit = "°C"; //declare unit of measurement
String message = "Temperature: " + String(temp) + unit; //combine strings
lcd.print(message); //display on LCD
```

The output on the LCD screen will be "Temperature: 25°C". As you can see, concatenating strings allows us to dynamically include the value of a variable in our output.

## Deep Dive

Concatenating strings in Arduino does have some limitations to keep in mind. First, the `String` data type in Arduino has a maximum length of 255 characters. If your combined strings exceed this limit, it could cause issues. Additionally, using `String` data type can take up more memory than other data types, so it's important to be mindful of memory usage in your program.

There are also some useful functions that can be used with concatenation in Arduino. The `concat()` function allows you to combine multiple strings at once, and the `remove()` function allows you to remove certain characters from a string. These functions can be helpful for more complex string manipulation.

## See Also

To learn more about string concatenation and other useful functions in Arduino, check out these resources:

- [Official Arduino String Reference](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [Arduino String Concat Function](https://www.arduino.cc/reference/en/language/functions/communication/serial/concat/)
- [Arduino String Remove Function](https://www.arduino.cc/reference/en/language/functions/communication/serial/remove/)