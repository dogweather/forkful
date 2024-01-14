---
title:    "Arduino recipe: Finding the length of a string"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## Why

If you are new to programming with Arduino, you may be wondering why you would need to know how to find the length of a string. The length of a string is important when manipulating data or when working with user input. Understanding how to find the length of a string can also help you with other programming tasks, making it a valuable skill to have in your coding arsenal.

## How To

Finding the length of a string may seem daunting at first, but with the right tools, it can be a relatively simple task. In order to find the length of a string in Arduino, you can use the built-in function "strlen()". This function takes in a string as a parameter and returns the number of characters in that string.

Let's see an example of how to use the "strlen()" function in Arduino:

```
Arduino void setup(){
    Serial.begin(9600); //initialize serial communication
    String myString = "Hello World!"; //define a string
    int stringLength = 0; //variable to hold the string length
    stringLength = strlen(myString); //use strlen function to find length
    Serial.print("The length of the string is: "); //print a message
    Serial.println(stringLength); //print the length of the string
}

void loop(){
    //do nothing
}
```

In this example, we first initialize the serial communication and define a string variable. Then, we use the "strlen()" function to find the length of the string and store it in a variable. Finally, we print a message along with the string length to the serial monitor.

If we upload this code to our Arduino board and open the serial monitor, we will see the output:

```
The length of the string is: 12
```

This means that our string "Hello World!" has a length of 12 characters.

## Deep Dive

To better understand how the "strlen()" function works, let's take a deeper dive into string manipulation in Arduino. A string in Arduino is an array of characters, with each character represented by a single byte value. Therefore, the length of a string can be determined by counting the number of bytes it occupies in memory.

There are also other methods to find the length of a string in Arduino, such as using a for loop to iterate through the characters of the string and incrementing a counter variable for each character. However, using the "strlen()" function is more efficient and convenient.

It is important to note that the "strlen()" function will only give accurate results if the string is properly null-terminated. This means that a null character ("\0") must be manually added at the end of the string to indicate the end of the string. If the string is not null-terminated, the "strlen()" function may return an incorrect length.

## See Also
- [String manipulation in Arduino](https://www.arduino.cc/reference/en/language/variables/data-types/stringobject/)
- [Working with character arrays in Arduino](https://www.mikethebot.com/microcontrollers/arduino-string2array)
- [Introduction to strings in Arduino](https://create.arduino.cc/projecthub/PatelDarshil/strings-in-arduino-3fdc2e)