---
title:                "Searching and replacing text"
html_title:           "C++ recipe: Searching and replacing text"
simple_title:         "Searching and replacing text"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## What & Why?
Searching and replacing text is a common task in programming where a specific string of characters is found and replaced with another string, either throughout a single document or across multiple documents. Programmers often perform this task to make changes to their code more efficiently or to fix errors in their code.

## How to:
```
C++ code for searching and replacing text:
#include <iostream>
#include <string>

using namespace std;

int main() {
    string sentence = "Hello World!";
    string newSentence = sentence.replace(sentence.find("World"), 5, "Universe");
    cout << newSentence << endl;
}
```
Output: "Hello Universe!"

## Deep Dive:
Searching and replacing text has been a crucial feature for programmers since the beginning of programming languages. It allows for quick and efficient changes to be made to code without having to manually go through every line. As technology has advanced, searching and replacing text has become even more powerful, with support for regular expressions, case-sensitive matching, and batch processing.

While searching and replacing text is mainly performed using built-in functions or tools in programming languages, there are also alternative programs and tools dedicated solely to this task. These include command-line utilities such as grep and sed, as well as text editors like Sublime Text and Notepad++.

When it comes to implementation, the method used for searching and replacing text may vary depending on the programming language or tool being used. Some may use algorithms such as Knuth-Morris-Pratt or Boyer-Moore for more efficient search and replace operations.

## See Also:
- [String class in C++](https://www.geeksforgeeks.org/string-class-in-c/)
- [Regular Expressions in C++](https://www.geeksforgeeks.org/regular-expression-in-c/)
- [Grep and Sed utilities](https://www.gnu.org/software/grep/manual/grep.html)