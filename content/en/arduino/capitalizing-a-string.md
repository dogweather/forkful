---
title:                "Capitalizing a string"
html_title:           "Arduino recipe: Capitalizing a string"
simple_title:         "Capitalizing a string"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Why

Capitalizing strings may seem like a minor aspect of programming, but it can greatly improve the functionality and readability of your code. Whether you are creating a text-based game or parsing user input, properly capitalized strings can make a significant difference in the outcome.

## How To

To capitalize a string in Arduino, we can utilize the `toUpperCase()` function. This function takes in a string as an argument and converts all lowercase letters to uppercase.

```Arduino
String name = "john doe";
name.toUpperCase();
Serial.println(name); //Output: JOHN DOE 
```

In the above example, we first create a string variable named "name" and assign it the value of "john doe". Then, we use the `toUpperCase()` function to convert the string to all uppercase letters. Lastly, we use the `Serial` library to print out the modified string to the serial monitor.

But what if we only want to capitalize the first letter of the string? In this case, we can use the `substring()` function to extract the first character and then use the `toUpperCase()` function on it.

```Arduino
String name = "john doe";
String firstLetter = name.substring(0,1);
firstLetter.toUpperCase();
Serial.println(firstLetter); //Output: J
```

Notice how we used the `substring()` function to extract the first character, which is then converted to uppercase. This method can be useful when dealing with user inputs or creating personalized greetings.

## Deep Dive

While the above examples cover the basics of capitalizing a string in Arduino, there are more complex situations where a deeper understanding of strings is necessary. One important thing to note is that strings in Arduino are essentially an array of characters. This means we can use a `for` loop to iterate through each character and convert them individually.

```Arduino
String sentence = "this is a phone";
for (int i = 0; i < sentence.length(); i++) {
  if (sentence[i] == ' ') { //check for empty space
    sentence[i+1] = toupper(sentence[i+1]); //convert next character to uppercase
  }
}
Serial.println(sentence); //Output: This Is A Phone 
```

In the above code, we use the `length()` function to determine the number of characters in the string. Then, using a `for` loop, we go through each character and check if it is a space. If it is, we use the `toupper()` function to convert the next character to uppercase. This results in every word in the string being capitalized.

## See Also

For more information on manipulating strings in Arduino, check out the following resources:

- [String reference](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/)
- [Arrays in Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/array/)
- [Tutorials on string manipulation in Arduino](https://create.arduino.cc/projecthub/tags/strings)