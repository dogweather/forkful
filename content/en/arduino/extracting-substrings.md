---
title:                "Arduino recipe: Extracting substrings"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/extracting-substrings.md"
---

{{< edit_this_page >}}

## Why

Have you ever wanted to extract a specific section of text from a longer string? Perhaps you only need a certain section of a sensor reading or you want to parse through a long text message. Whatever your reason, being able to extract substrings in your Arduino programming can save you time and make your code more efficient.

## How To

In this tutorial, we will be using the substring() function in Arduino to extract portions of a string. Let's say we have a string that contains the temperature and humidity data from a sensor, separated by a comma. We want to extract just the temperature value and use it in our code.

First, we will declare our string variable and assign it a value:

```
ArduinoString data = "25.4, 60";
```

Next, we will use the substring() function to extract the temperature value. This function takes in two parameters - the starting index and the number of characters to extract.

```
int start = 0; //since our temperature value starts at index 0
int length = data.indexOf(","); //we want everything before the comma
int temperature = data.substring(start, length).toInt(); //converting the extracted string to an integer
```

We can now use the temperature value in our code. Here's a full example with the output:

```
ArduinoString data = "25.4, 60"; //declare and assign string variable
int start = 0;
int length = data.indexOf(",");
int temperature = data.substring(start, length).toInt(); //extracting and converting to int
Serial.println(temperature); //output: 25
```

## Deep Dive

The substring() function in Arduino allows us to easily extract substrings from a longer string. It takes in two parameters - the starting index and the number of characters to extract. This makes it very versatile and allows us to customize our extractions based on our specific needs.

It's important to note that the starting index is inclusive, meaning it will include the character at that index in the extraction. The ending index is exclusive, meaning it will not include the character at that index in the extraction.

Another useful function when extracting substrings is indexOf(). This function allows us to find the index of a specific character within a string. In our example, we used it to find the index of the comma separating the temperature and humidity values.

## See Also

- [Arduino substring() reference](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/substring/)
- [Arduino indexOf() reference](https://www.arduino.cc/reference/en/language/variables/data-types/string/functions/indexof/)
- [Tutorial: String manipulation in Arduino](https://www.arduino.cc/en/Tutorial/StringVariableTutorial)