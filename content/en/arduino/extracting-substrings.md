---
title:    "Arduino recipe: Extracting substrings"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Why
Have you ever encountered a situation where you only needed a part of a string of characters? For example, you have a long serial number but only need the last four digits. In Arduino programming, this process is called extracting substrings. It allows you to easily manage and use specific parts of a string without having to manipulate the entire string. In this blog post, we will explore why and how to extract substrings using Arduino.

## How To
To extract substrings in Arduino, we will be using the `substring()` function. This function takes in two parameters: the starting index and the ending index of the substring. The syntax for using this function is as follows:

```Arduino
myString.substring(startIndex, endIndex);
```

To better understand this, let's take a look at a coding example. Suppose we have a string called `serialNumber` that contains a serial number. We only need the last four digits of this number. We can use the `substring()` function to extract these digits as shown below:

```Arduino
String serialNumber = "123456789";
String lastDigits = serialNumber.substring(6, 9);
```

The code above will extract the last three digits of the `serialNumber` string starting from index 6 (the seventh character) and ending at index 9 (the tenth character). The resulting `lastDigits` string will contain the value "789". 

## Deep Dive
There are a few important things to note when using the `substring()` function. Firstly, the starting index is inclusive, meaning that the character at that index will also be included in the substring. However, the ending index is exclusive, meaning that the character at that index will not be included in the substring. This is why in the example above, we used `9` as the ending index instead of `10`.

Another important thing to consider is the index numbering in strings. In Arduino, strings are zero-indexed, meaning the first character of the string is at index 0, the second character at index 1, and so on. This means that to extract the last four digits of a string, the starting index would be the length of the string minus 4, and the ending index would be the length of the string. This can be seen in the following code:

```Arduino
String serialNumber = "123456789";
String lastDigits = serialNumber.substring(serialNumber.length() - 4, serialNumber.length());
```

Using the `serialNumber.length()` function, we can get the length of the string and use it to calculate the correct indexes for our substring.

Finally, it is important to note that the `substring()` function returns a new string and does not modify the original string. This means that if you want to use the extracted substring, you will need to assign it to a new string variable like we did in the examples above.

## See Also
For more information on the `substring()` function and string manipulation in general, check out the following links:

- [Arduino String Reference](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [Arduino String length() function](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/length/)
- [W3Schools String Substring() Function](https://www.w3schools.com/cpp/cpp_strings_substrings.asp)