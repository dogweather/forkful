---
title:    "Arduino recipe: Extracting substrings"
keywords: ["Arduino"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

# Why

Have you ever found yourself needing to extract specific parts of a string of text in your Arduino coding projects? Maybe you want to extract a name from an email address, or retrieve a date from a longer string of data. Whatever the reason may be, learning how to extract substrings in Arduino can be a useful skill to have in your coding toolkit.

# How To

First, let's take a look at an example of a string that we want to extract a substring from:

```
Arduino Programming is Fun!
```

Let's say we want to retrieve the word "Programming" from this string. We can do this by using the `substring()` function in Arduino. This function takes two parameters - the starting index of the substring and the length of the substring.

```
Arduino Programming is Fun!
0123456789
```

In this case, the index of the first letter in "Programming" is 8 and the length of the word is 11. So, our code would look like this:

```arduino
String text = "Arduino Programming is Fun!";
String extracted = text.substring(8, 11);
Serial.println(extracted); // prints "Programming"
```

We can also use variables to make our code more dynamic. For example, let's say we want to extract a substring from a user inputted email address:

```arduino
String email = Serial.readString();
String username = email.substring(0, email.indexOf("@"));
Serial.println(username); // prints the username before the "@" symbol
``` 

# Deep Dive

The `substring()` function in Arduino can also take negative numbers as parameters. This can be useful if you want to extract a string from the end of a larger string.

For example, let's say we want to extract the last 4 digits of a phone number:

```arduino
String phone = "123-456-7890";
String extracted = phone.substring(-4);
Serial.println(extracted); // prints "7890"
```

It's important to note that if you use a negative number as the starting index, the substring will begin from the end of the string. So, using `-4` as the starting index will start the substring from the 4th character from the end.

# See Also

For more information on the `substring()` function in Arduino, check out the official Arduino documentation:
- https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/