---
title:                "Arduino recipe: Using regular expressions"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/using-regular-expressions.md"
---

{{< edit_this_page >}}

##Why

Have you ever found yourself needing to search for a specific pattern of characters within a large chunk of text? Regular expressions are a powerful tool that allows you to do just that. They make it easier to find, replace, and manipulate text in your Arduino programs.

##How To

To use regular expressions in Arduino, we first need to include the RegularExpressions library. This can be done by navigating to Sketch > Include Library > RegularExpressions. Once the library is included, we can begin using regular expressions in our code.

Let's take a simple example of searching for a specific word in a string. Say we have a string variable called "sentence" and we want to check if it contains the word "Arduino". We can use the ```match()``` function from the RegularExpressions library to achieve this:

```
#include <RegularExpression.h>

String sentence = "I love my Arduino!";

if (match(sentence, "Arduino")) {
  Serial.println("The word Arduino was found!");
} 
```
The ```match()``` function returns a boolean value, so if the word "Arduino" is found in the sentence, the if statement will evaluate to true and the corresponding message will be printed to the serial monitor.

Regular expressions also allow us to search for more complex patterns using wildcards, anchors, and quantifiers. Let's say we want to check if our string contains any numbers. We can use the ```[0-9]``` wildcard, which represents any digit from 0 to 9.

```
#include <RegularExpression.h>

String sentence = "There are 10 types of people in the world.";

if (match(sentence, "[0-9]")) {
  Serial.println("There are numbers in the sentence!");
} 
```

The ```[0-9]``` wildcard will match any single digit in the sentence, so even though there are multiple numbers in the sentence, the if statement will evaluate to true as long as it finds at least one digit.

##Deep Dive 

Regular expressions may seem daunting at first, but once you understand the different components and their meanings, they can greatly simplify your code. Some of the commonly used components in regular expressions include:

- Wildcards: These are characters that can match any single character in a string. Examples include ```[a-z]``` (matches any lowercase letter) and ```[0-9]``` (matches any digit).

- Anchors: These indicate the beginning or end of a string. The most commonly used anchors are ```^``` (matches the beginning of a string) and ```$``` (matches the end of a string).

- Quantifiers: These indicate how many times a character or group of characters should be repeated. Some examples include ```+``` (matches one or more occurrences), ```*``` (matches zero or more occurrences), and ```?``` (matches zero or one occurrence).

For a more comprehensive guide on the different components and syntax of regular expressions, check out this [tutorial](https://www.regular-expressions.info/tutorial.html). Also, remember to test your regular expressions using online [regex testers](https://regexr.com/) before implementing them in your code.

##See Also

- [Official Arduino Regular Expressions Documentation](https://www.arduino.cc/reference/en/libraries/regularExpressions/)
- [Regular Expressions Reference Sheet](https://www.regular-expressions.info/refquick.html)
- [Interactive Regular Expressions Tutorial](https://regexone.com/)