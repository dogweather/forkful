---
title:                "टेक्स्ट खोज और बदलाव करना"
html_title:           "C++: टेक्स्ट खोज और बदलाव करना"
simple_title:         "टेक्स्ट खोज और बदलाव करना"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Kyun

Kabhi kabhi humein apne code mein kuch changes karne ki zaroorat hoti hai. Isme se ek kaam hai text ko search karke usay replace karna. Yeh kaam karne se hum apne code ko improve kar sakte hai aur code ke errors ko fix kar sakte hai.

## Kaise Karein

```C++
#include <iostream>
#include <string>

using namespace std;

int main() 
{
  // Original string
  string text = "Hello world!";

  // Search and replace
  text.replace(text.find("world"), 5, "universe");

  // Output new string
  cout << text << endl;

  return 0;
}

// Output:
// Hello universe!
```

Is code mein humne apne original string mein se "world" ko search kiya aur usay "universe" se replace kiya. Is tarah hum apne code mein koi bhi word ya phrase ko aasani se replace kar sakte hai.

## Deep Dive

Searching and replacing text is a useful function that saves time and effort when it comes to making changes in code. It allows us to replace multiple instances of a word or phrase with just one command. In C++, we can use the `replace()` function to search and replace text within a string.

This function takes three arguments: the position to start the search from, the number of characters to replace, and the replacement string. By using the `find()` function, we can determine the starting position of the word or phrase we want to replace. Then, we can specify the number of characters to replace, which is the length of the word or phrase. Finally, we can specify the replacement string. 

## Dekhain Bhi

- https://www.geeksforgeeks.org/stringreplace-function-in-c-stl/
- https://www.tutorialspoint.com/cplusplus/cpp_strings.htm
- https://www.programiz.com/cpp-programming/library-function/c-string/replace