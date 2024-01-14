---
title:    "C++ recipe: Searching and replacing text"
keywords: ["C++"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/en/cpp/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## Why 
There are many reasons why someone would need to search and replace text in their code. Some common examples include fixing errors, standardizing variable names, or implementing new features. Searching and replacing text can save time and effort when making large scale changes to code.

## How To
Searching and replacing text can be done in any text editor or IDE. Most editors have a built-in find and replace function, but this can also be done through a simple C++ program. Let's take a look at an example using the C++ string library:

```
#include <iostream>
#include <string>

int main() {
    //Initialize string
    std::string sentence = "Hello world!";

    //Replace "world" with "universe"
    sentence.replace(sentence.find("world"), 5, "universe");

    //Output the updated string
    std::cout << sentence << std::endl;

    return 0;
}
```
Output: Hello universe!

As you can see, the string's "find" function is used to locate the position of the word "world" and the "replace" function is used to replace it with "universe". This can be done for multiple instances of the word as well.

Another approach is to use regular expressions. This allows for more advanced searching and replacing with specific patterns. Here's an example using the C++ regex library:

```
#include <iostream>
#include <regex>

int main() {
    //Initialize string
    std::string sentence = "The quick brown fox jumps over the lazy dog.";

    //Replace all vowels with the letter "a"
    sentence = std::regex_replace(sentence, std::regex("[aeiou]"), "a");

    //Output the updated string
    std::cout << sentence << std::endl;

    return 0;
}
```
Output: Tha qaack braan fax jamps avar tha lazy dag.

## Deep Dive
Searching and replacing text can also be done using command line tools like sed or using scripting languages like Python or Perl. It's important to carefully consider the text you are searching for and the text you want to replace it with, as well as using caution when using regular expressions.

It's also helpful to use version control when making large scale changes to code, as it allows for easy reverting in case of any errors.

## See Also
- [C++ string library documentation](https://www.cplusplus.com/reference/string/)
- [C++ regex library documentation](https://www.cplusplus.com/reference/regex/)
- [Sed tutorial](https://www.geeksforgeeks.org/sed-command-in-linux-unix-with-examples/)
- [Python string methods](https://www.w3schools.com/python/python_strings.asp)