---
title:    "C recipe: Extracting substrings"
keywords: ["C"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/c/extracting-substrings.md"
---

{{< edit_this_page >}}

##Why

Have you ever come across a situation where you need to take a part of a string and use it as a separate entity in your program? This is where extracting substrings comes in handy. By extracting substrings, you can take a portion of a string and manipulate it to fit your specific needs.

##How To

To extract substrings in C, we will be using the "```strncpy()```" function. This function allows us to copy a part of the string into a new string. Let's take a look at an example:

```
char str[] = "Hello World";
char sub[6];
strncpy(sub, str + 6, 5);
printf("Substring: %s", sub);
```

In the above code, we first declare a string "str" with the value "Hello World". Then, we declare a new string "sub" with a size of 6, as our substring will contain 5 characters plus the null terminator. Next, we use the ```strncpy()``` function to extract the substring starting from the 6th character in "str" and copy it into "sub". Finally, we print out the substring using the ```printf()``` function.

The output of this code will be: "Substring: World". Notice how we were able to extract the word "World" from the original string and use it as a separate entity.

##Deep Dive

There are a few parameters that we need to understand when using the ```strncpy()``` function. The first parameter is the destination string, in our case "sub". The second parameter is the source string, in our case "str". The third parameter is the number of characters we want to copy, which is 5 in our example. And finally, the fourth parameter is the maximum length of the destination string. This is important because if the source string contains more than the specified number of characters, the destination string will not have a null terminator at the end. Therefore, it is important to specify the maximum length of the destination string in order to avoid any unexpected errors.

Another important thing to note is that when using "```strncpy()```", it will always copy the specified number of characters, even if the source string is shorter. This can result in your substring not having a null terminator at the end. To avoid this, it is recommended to manually add a null terminator after using the ```strncpy()``` function, like so:

```
sub[5] = '\0';
```

##See Also

If you want to learn more about string manipulation in C, check out the following resources:

- [String Manipulation in C: A Beginner's Guide](https://www.geeksforgeeks.org/string-manipulations-in-c-without-using-library-functions/)
- [C Strings and String Functions](https://www.tutorialspoint.com/cprogramming/c_strings.htm)
- [String data type in C](https://www.programiz.com/c-programming/c-strings)

Now that you have a better understanding of how to extract substrings in C, you can use this knowledge in your future projects. Happy coding!