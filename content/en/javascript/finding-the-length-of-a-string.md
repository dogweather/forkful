---
title:    "Javascript recipe: Finding the length of a string"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## Why
As a beginner in programming, one of the most common tasks you will encounter is finding the length of a string. Whether you're building a simple text-based game or working on a complex web application, knowing how to find the length of a string is essential in order to manipulate and process text data.

## How To
To find the length of a string in Javascript, we can use the built-in method ```length```. This method returns the number of characters in a string, including any whitespace or special characters.

Let's look at a simple example:

```Javascript
let string = "Hello World";
console.log(string.length); // Output: 11
```
In this code block, we have declared a string variable called ```string``` and assigned it the value of "Hello World". Using ```console.log()```, we can print out the length of this string, which is 11 characters. Keep in mind that spaces and punctuation also count towards the length of a string.

We can also find the length of a string that is stored in a variable or returned from a function. See the code below:

```Javascript
let username = "john123";
let lengthOfUsername = username.length;
console.log(lengthOfUsername); // Output: 7

function findLength(string) {
    return string.length;
}

console.log(findLength("Javascript is fun")); // Output: 17
```

In the first example, we declare a variable ```username``` with the value of "john123" and then use the ```length``` method to find the length of the string and store it in a variable called ```lengthOfUsername```. In the second example, we have a function ```findLength``` which takes in a string parameter and returns its length. We can pass in any string and the function will return its length.

## Deep Dive
Behind the scenes, the ```length``` method works by looping through each character in the string and counting it. This loop stops when it reaches the end of the string, and the total number of characters is returned. This method is very efficient and can handle strings of any length.

It's important to note that the ```length``` method is case-sensitive, so uppercase and lowercase letters will have different lengths. For example, the string "Hello" has a length of 5, while "hello" has a length of 4.

Another thing to keep in mind is that the ```length``` method can only be used on strings. If you try to use it on a number or boolean value, it will return an error.

## See Also
- [Mozilla Developer Network: String Length](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [W3Schools: Javascript Strings](https://www.w3schools.com/js/js_strings.asp)
- [GeeksforGeeks: Finding Length of a String in Javascript](https://www.geeksforgeeks.org/how-to-find-the-length-of-a-string-in-javascript/)

By understanding how to find the length of a string, you will have a solid foundation for working with text data in your Javascript projects. So go ahead and start practicing finding the length of strings to enhance your coding skills!