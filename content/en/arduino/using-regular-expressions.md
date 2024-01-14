---
title:    "Arduino recipe: Using regular expressions"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Why 

Regular expressions are powerful tools that allow programmers to efficiently manipulate and extract data from text. They can save time and effort when handling large amounts of data or performing complex searches. In the world of Arduino programming, regular expressions can be especially useful for creating robust and flexible applications. 

## How To

Before we dive into the world of regular expressions, it's important to have a basic understanding of what they are and how they work. In simple terms, a regular expression or "regex" is a sequence of characters that form a search pattern. This pattern can then be used to match and manipulate strings of text.

To use regular expressions in Arduino programming, we need to first include the "Regex" library. We can do this by going to "Sketch" > "Include Library" > "Regex". Once the library is added, we can then start using regular expressions in our code.

To demonstrate, let's create a regex that will search for specific phone numbers in a string. We can do this by using the "match()" function and specifying our search pattern using regular expression syntax. Here's an example:

```
ArduinoRegex regex;  // create an instance of the Regex class
String phoneNumber = "+1 (123) 456-7890";
String pattern = "\\+?([0-9]{1,3})\?([0-9]{3})-?[0-9]{3}-?[0-9]{4}";  // regex pattern for phone numbers
if (regex.match(phoneNumber, pattern)) {  // check if the phone number matches the pattern
  Serial.println("Valid phone number!");  // if it does, print a success message
} else {
  Serial.println("Invalid phone number.");  // otherwise, print an error message
}
```

In this example, we're searching for phone numbers in the format of "+1 (123) 456-7890". The regex pattern we've provided will match any number that follows this format, including variations with or without the country code. 

Now let's say we want to extract the area code from a given phone number. We can do this by using the "extract()" function, which will return the matched substring. Here's an example:

```
String areaCode = regex.extract(phoneNumber, "\\(([0-9]{3})\\)");
Serial.println("The area code is " + areaCode);  // output: The area code is 123
```

In this code, we are using the expression "\\(([0-9]{3})\\)" to match three digits enclosed in parentheses. This will result in the area code being extracted from the phone number and printed to the serial monitor.

## Deep Dive

While the examples we've shown are relatively simple, regular expressions can be extremely complex and powerful. They make use of wildcard characters, modifiers, and other special syntax to create flexible and precise search patterns. It is important to familiarize yourself with these different elements and their meanings in order to effectively use regular expressions in your code.

For a more in-depth understanding of regular expressions, there are many online resources and tutorials available. It's also helpful to experiment with different patterns and see how they affect the outcome of your code.

## See Also
- Official Arduino documentation on Regular Expressions: https://www.arduino.cc/reference/en/libraries/regex/
- Regex tutorial on W3Schools: https://www.w3schools.com/jsref/jsref_obj_regexp.asp
- Basic regular expression cheat sheet: https://www.cheatography.com/davechild/cheat-sheets/regular-expressions/