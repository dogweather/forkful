---
title:    "Arduino recipe: Capitalizing a string"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why
Capitalizing strings may seem like a trivial task, but it can actually be a very useful tool in various projects. Whether you are creating a user interface with user input or simply want to display text in a more visually appealing way, capitalizing strings can add a nice touch to your Arduino projects.

## How To
To capitalize a string in Arduino, we can use a built-in function called `toUpperCase()`. This function converts all lowercase letters in a string to uppercase. Let's see how it works with an example:

```
Arduino String input = "hello world";
input.toUpperCase();  // calling the toUpperCase() function
Serial.println(input);  // prints "HELLO WORLD" 
```

We can also use string concatenation to create a new string with the capitalized version:

```
Arduino String input = "hello world";
String capitalized = "The capitalized version is: " + input.toUpperCase();
Serial.println(capitalized);  // prints "The capitalized version is: HELLO WORLD"
```

## Deep Dive
Now, let's take a deeper look at the `toUpperCase()` function. The actual code for this function can be found in the `WString.cpp` file in the Arduino core library. Here is the simplified version of the function:

```
// function definition
String String::toUpperCase(){
    String result = "";
    for(int i = 0; i < length(); i++){  // loops through each character in the string
        if(buffer[i] >= 'a' && buffer[i] <= 'z'){  // checks if the character is lowercase
            result += buffer[i] + 'A' - 'a';  // subtracts the ASCII values to convert to uppercase
        }
        else{
            result += buffer[i];  // if it's not lowercase, add the character as is
        }
    }
    return result;
}
```

As we can see, the `toUpperCase()` function uses ASCII values to convert lowercase letters to uppercase. It also checks for non-alphabetic characters and adds them to the new string as is.

## See Also
- [Arduino String Reference](https://www.arduino.cc/reference/en/language/variables/data-types/stringfunctions/touppercase/)
- [ASCII Table](https://www.rapidtables.com/code/text/ascii-table.html)