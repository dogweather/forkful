---
title:    "C++ recipe: Writing to standard error"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/cpp/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Why
As a programmer, you may have come across the term "standard error" while debugging your code. But have you ever wondered why we engage in writing to standard error? In this blog post, we will explore the importance of writing to standard error and how it can be useful for developers.

## How To
Writing to standard error in C++ is a standard way of displaying error messages or debugging information to the user. This is achieved by using the `std::cerr` object, which is specifically designed for error output. Let's take a look at an example:

```C++
#include <iostream>

int main() {
    int num = 0;
    std::cout << "Enter a number: ";
    std::cin >> num;
    
    if (num % 2 == 0) {
        std::cerr << "Even number entered!" << std::endl;
    }
    
    return 0;
}
```

In this code, we are using `std::cerr` to output a message if the user enters an even number. This way, we can alert the user about any potential errors in our code. The output for this code would be:

```
Enter a number: 4
Even number entered!
```

As you can see, the message was displayed on a separate line, indicating it was written to standard error. This is useful for distinguishing between regular output and error messages.

## Deep Dive
So why do we use `std::cerr` instead of `std::cout`? The main difference between the two is that `std::cerr` is unbuffered while `std::cout` is buffered. This means that `std::cerr` immediately displays the output to the user, whereas `std::cout` may delay the display until a certain condition is met. In the case of error messages, we want them to be displayed immediately so that the user is aware of any issues with the program.

It's also important to note that `std::cerr` is not limited to just error messages. You can use it to output any kind of debugging information or progress updates to the user. This makes it a useful tool for developers to track the execution of their code and identify any potential issues.

## See Also
- [C++ Standard Library](https://en.cppreference.com/w/cpp/header)
- [Introduction to Standard Input and Output in C++](https://www.geeksforgeeks.org/introduction-input-output-header-files-c-cpp/)

Writing to standard error may seem like a simple concept, but it is an essential aspect of programming that can greatly improve the debugging process. So the next time you encounter an error in your code, remember the `std::cerr` object and use it to your advantage.