---
title:                "Arduino recipe: Using regular expressions"
simple_title:         "Using regular expressions"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

## Why
Regular expressions, also known as regex, are a powerful tool for pattern matching and text manipulation. They allow for more complex and versatile string parsing, which can be useful for a variety of applications. If you are an Arduino programmer, learning how to use regular expressions can greatly enhance your programming skills and make your code more efficient.

## How To
Using regular expressions in Arduino is possible through the use of the `Regex` library. First, include the library in your sketch by adding `#include <Regex.h>` at the top. Then, declare a `Regex` object and provide it with the pattern you want to match.

```
Arduino digitalWrite(LED_BUILTIN, HIGH);
  Serial.begin(9600);
  Regex regex("Hello\\s(Arduino)");
```

In the code above, we have declared a `Regex` object that will match the pattern "Hello Arduino". The "\\s" is an escape character for the white space in the pattern. You can use different symbols and characters to specify the pattern you want to match.

Next, we can use the `match()` function to check if a string matches our regex pattern. The `match()` function returns a boolean value of either true or false.

```
String text = "Hello Arduino";

if(regex.match(text)){
  Serial.println("Pattern matched!");
} else {
  Serial.println("Pattern not found.");
}
```

In this example, the code will output "Pattern matched!" since the string "Hello Arduino" matches our regex pattern.

## Deep Dive
Regular expressions offer more advanced features such as character sets, quantifiers, and grouping. Character sets allow you to specify a range of characters to be matched, while quantifiers allow you to specify how many times a character or set of characters should be repeated. Grouping allows you to organize patterns and extract specific parts of the matched string.

Another useful feature of regular expressions is the ability to replace certain patterns with other strings. This is possible through the `replace()` function. You can use `replace()` to modify strings or even extract parts of a string.

```
//replace hello with hi and extract the name
String text = "Hello, my name is John";
regex.replace(text, "Hi, my name is $1");

//output: Hi, my name is John
```

There are many other advanced techniques and tips for using regular expressions in Arduino programming. It's a powerful tool that can greatly enhance your string manipulation capabilities.

## See Also
- [Arduino Reference: Regex Library](https://www.arduino.cc/reference/en/libraries/regex/)
- [RegExr: Learn, Build, & Test Regular Expressions](https://regexr.com/)
- [Regular-Expressions.info: A site dedicated to regular expressions](https://www.regular-expressions.info/)