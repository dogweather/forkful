---
title:                "C++ recipe: Reading command line arguments"
programming_language: "C++"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Why

In the world of software development, being able to read and parse command line arguments is an essential skill. It allows for easy customization and flexibility, and can even be the difference between a user-friendly program and a frustrating one. In this blog post, we will take a deep dive into reading command line arguments in C++, and why it is a valuable skill to have.

## How To

Reading command line arguments in C++ is a fairly straightforward process. To get started, we need to include the <iostream> and <string> libraries.

```
#include <iostream>
#include <string>
```

Next, we declare our main method with two parameters, argc and argv. These parameters will hold the number of arguments and the actual arguments, respectively.

```
int main(int argc, char* argv[])
```

To read and process the arguments, we can use a for loop. The variable i will start at 1, as the first argument (argv[0]) will always be the name of our program. We can then use the std::string type to store each argument for further manipulation.

```
for(int i = 1; i < argc; i++){
    std::string arg = argv[i];
    // do something with the argument
}
```

Now, let's say we have a program called "greeting" and we want to take in two arguments - a name and a message. Our command line would look something like this:

```
./greeting John Hello
```

With our for loop, we can store these arguments and print them out using the std::cout function.

```
for(int i = 1; i < argc; i++){
    std::string arg = argv[i];
    std::cout << arg << " ";
}
```

This would result in an output of:

```
John Hello
```

## Deep Dive

There are many other functionalities and techniques that can be used when reading command line arguments in C++. One useful function is the std::stoi() function, which can convert a string argument to an integer. This is especially helpful if your program requires numerical input from the user.

Additionally, you can use the argc and argv parameters in other areas of your program, such as error handling or conditional statements. By understanding the basics of reading command line arguments, you can incorporate them into your code in creative and efficient ways.

## See Also

- [C++ Command Line Arguments](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)
- [The Beginner's Guide to Command Line Arguments in C++](https://www.educba.com/command-line-arguments-in-cpp/)
- [std::stoi() function](https://www.cplusplus.com/reference/string/stoi/)

By mastering the skill of reading command line arguments in C++, you can enhance the functionality and user experience of your programs. With these coding examples and tips, you can get started on your journey to becoming a command line argument expert. Happy coding!