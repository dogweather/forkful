---
title:                "C++ recipe: Deleting characters matching a pattern"
simple_title:         "Deleting characters matching a pattern"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/en/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Why 

Have you ever come across a situation where you need to delete certain characters from a string that match a specific pattern? Maybe you want to remove all punctuation marks from a sentence or delete all numbers from a string. This is where the concept of deleting characters matching a pattern comes in handy.

## How To 

To delete characters matching a pattern in C++, we will be using the `std::remove_if()` function from the `<algorithm>` header. This function takes in three parameters - beginning iterator, ending iterator, and a predicate function. The beginning and ending iterators represent the range of characters in the string while the predicate function determines which characters to remove.

Let's take a look at an example where we want to remove all lowercase vowels from a string:

```
#include <iostream>
#include <algorithm>

using namespace std;

bool isVowel(char c){
    // predicate function to check if a character is a lowercase vowel
    return (c=='a' || c=='e' || c=='i' || c=='o' || c=='u');
}

int main(){
    // input string
    string str = "Hello World";

    // using std::remove_if() function to delete characters matching a pattern
    str.erase(std::remove_if(str.begin(), str.end(), isVowel), str.end());

    // output string
    cout << str << endl;

    return 0;
}
```

Output:
```
Hll Wrld
```

As we can see, the `erase()` function is used along with `remove_if()` to delete the characters that match the pattern specified by the predicate function.

## Deep Dive

The `remove_if()` function works by moving all the elements that satisfy the predicate function to the end of the range and returning an iterator pointing to the beginning of the removed elements. The `erase()` function then erases these elements from the string, effectively deleting the characters matching the pattern.

One important thing to note is that the `remove_if()` function does not physically remove the characters from the string, it just moves them to the end of the range. The `erase()` function then erases these elements and shifts all the remaining characters to the left, giving the illusion of deleted characters.

## See Also

- [How to remove elements from a string in C++](https://www.geeksforgeeks.org/remove-elements-from-a-string-in-cpp/)
- [std::remove_if() function in C++](https://en.cppreference.com/w/cpp/algorithm/remove)
- [Erase-Remove idiom in C++](https://en.wikipedia.org/wiki/Erase%E2%80%93remove_idiom)