---
title:                "Arduino recipe: Finding the length of a string"
programming_language: "Arduino"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/arduino/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## Why

In this blog post, we will explore one of the fundamental concepts of programming - finding the length of a string. Understanding how to determine the length of a string is essential for any programmer, especially for beginners. Having this knowledge will open up a world of possibilities in terms of data manipulation and processing. So, let's dive into the world of strings and learn how to find their length in Arduino programming.

## How To

To find the length of a string in Arduino, we will use the `strlen()` function from the standard C library. This function takes in a string as its parameter and returns its length as an integer value.

```Arduino
String myString = "Hello World";
int length = strlen(myString.c_str()); //convert String to char array
Serial.println(length); //output: 11
```

In the above example, we first declared a string variable called `myString` with the value "Hello World". Then, we use the `c_str()` method to convert the string into a character array, which the `strlen()` function can work with. Finally, the returned length value is printed using `Serial.println()`. 

Another approach to finding the length of a string is to use a `for` loop to iterate through the characters of the string and increment a counter variable until the null character `\0` is encountered.

```Arduino
String myString = "Hello World";
int length = 0; //initialize counter variable

for(int i=0; myString[i]!='\0'; i++){
  length++;
}

Serial.println(length); //output: 11
```

Both methods will give you the same result, but using the `strlen()` function is more efficient and less error-prone.

## Deep Dive

Now, let's take a deeper look at how the `strlen()` function works. It reads each character of the string until it reaches the null character `\0`, which indicates the end of the string. It then returns the number of characters read as the length of the string.

It is important to note that the length of a string does not include the null character `\0`. For example, the string "Hello" has a length of 5, even though it has 6 characters. This is because the null character is only used to terminate the string and is not considered a part of it.

Additionally, the `strlen()` function only works for null-terminated strings, which means the last character of the string must always be a null character. If the string is not properly terminated, the `strlen()` function may return an incorrect length value.

## See Also

- [Arduino String Reference](https://www.arduino.cc/reference/en/language/variables/data-types/string/)
- [C Strings and String Library](https://www.cprogramming.com/tutorial/c/lesson9.html)
- [Null Terminated Strings](https://www.cs.cmu.edu/~guna/15-123S11/Lectures/Lecture05.pdf)