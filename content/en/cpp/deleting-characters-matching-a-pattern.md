---
title:                "C++ recipe: Deleting characters matching a pattern"
programming_language: "C++"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

##Why 

Deleting characters matching a certain pattern can be useful for data cleaning and manipulation tasks. For example, if you have a dataset with inconsistent formatting or unwanted characters, you can use this method to remove them and make your data more uniform and usable.

##How To 

To delete characters matching a pattern in C++, you can use the `erase()` function from the `<string>` library. This function takes in two arguments - the starting position and the number of characters to be deleted. Here's a simple example:

```C++
#include <iostream> 
#include <string> 

using namespace std; 

int main() 
{ 
    string sentence = "Hello, World!"; 
    sentence.erase(7, 1); 
    cout << sentence << endl; 
    return 0; 
} 
```

Output: Hello World!

In this example, we have a string "Hello, World!" and we use the `erase()` function to remove the comma after "Hello". Note that the first character has an index of 0, so 7 represents the 8th character in the string. The second argument, 1, specifies that we want to delete 1 character. 

You can also use the `<algorithm>` library and the `remove_if()` function to delete characters based on a specific condition. Here's an example that removes all the vowels from a string:

```C++
#include <iostream> 
#include <string> 
#include <algorithm> 

using namespace std; 

int main() 
{ 
    string sentence = "This is a test sentence"; 
    sentence.erase(remove_if(sentence.begin(), sentence.end(), [](char c) 
        { return (c == 'a' || c == 'e' || c == 'i' || c == 'o' || c == 'u');}), 
        sentence.end()); 
    cout << sentence << endl; 
    return 0; 
} 
```

Output: Ths s  tst sntnc

Here, we use a lambda function to specify the condition for deletion - in this case, any character that is a vowel will be removed. 

##Deep Dive 

The `erase()` function works by modifying the original string, so it's important to use it with caution. If you want to keep the original string intact, you can use the `substr()` function to extract the desired characters and create a new string. For example:

```C++
#include <iostream> 
#include <string> 

using namespace std; 

int main() 
{ 
    string sentence = "Hello, World!"; 
    string new_sentence = sentence.substr(0, 7); 
    cout << new_sentence << endl; 
    return 0; 
} 
```

Output: Hello,

Here, we use the `substr()` function to extract the first 7 characters of the string "Hello, World!" and store it in a new string variable called `new_sentence`.

##See Also 

- [C++ erase() function](https://www.geeksforgeeks.org/c-stl-erase-function/)
- [C++ remove_if() function](https://www.geeksforgeeks.org/remove_if-in-cpp/)
- [C++ substr() function](https://www.cplusplus.com/reference/string/string/substr/)

By using the `erase()` function in C++, you can easily delete characters matching a specific pattern and clean up your data. However, it's important to be careful and use it appropriately to avoid unintended modifications to your strings. Happy coding!