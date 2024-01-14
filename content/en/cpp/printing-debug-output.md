---
title:                "C++ recipe: Printing debug output"
simple_title:         "Printing debug output"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why
Debugging is an essential part of the software development process. It helps us identify and fix potential errors and issues in our code. One useful tool in the debugging process is printing debug output. It allows us to see the values of variables and the flow of our program, making it easier to pinpoint the source of a bug. In this blog post, we will explore the why and how of printing debug output in C++.

## How To

To print debug output in C++, we can use the `cout` statement from the standard library's `iostream` header. This statement allows us to display the value of a variable or a custom message on the console. Let's look at some examples:

```C++
#include <iostream>

using namespace std;

int main(){
  int number = 5;
  cout << "The value of number is: " << number << endl; //output: The value of number is: 5
}
```

In the above code, we use the `cout` statement to print the value of the `number` variable. We can also use multiple `cout` statements to print out different values or messages in a single line, as shown below:

```C++
#include <iostream>

using namespace std;

int main(){
  int age = 25;
  int weight = 150;
  cout << "Age: " << age << ", Weight: " << weight << endl; //output: Age: 25, Weight: 150
}
```

Additionally, we can use conditional statements and loops to control when and how often we print out debug output. This allows us to narrow down our focus on specific parts of our code and analyze them in more detail.

## Deep Dive

Printing debug output not only helps us identify and fix bugs, but it also provides insight into the flow of our program. By printing out the values of variables at various stages of our code, we can track the changes and better understand how our program is executing. This is especially useful for larger and more complex programs.

However, it is important to note that excessive use of debug output can clutter the console and make it difficult to spot important information. It is recommended to use it sparingly and strategically.

Another helpful tip is to add additional information to our debug output, such as the name of the variable, to make it easier to track and understand. This can be done by using the `endl` statement to add line breaks between outputs.

## See Also

- [Debugging in C++](https://www.cplusplus.com/debugging/)
- [Using cout for debugging](https://stackoverflow.com/questions/12796219/using-cout-for-debugging)

Debugging is an essential skill for any programmer, and printing debug output is just one of the many tools we can use to make the process easier and more efficient. So next time you encounter a bug in your C++ program, don't forget to add some `cout` statements to help you out!