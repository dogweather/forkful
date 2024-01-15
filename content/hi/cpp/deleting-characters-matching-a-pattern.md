---
title:                "पैटर्न से मेल खाने वाले अक्षरों को हटाना"
html_title:           "C++: पैटर्न से मेल खाने वाले अक्षरों को हटाना"
simple_title:         "पैटर्न से मेल खाने वाले अक्षरों को हटाना"
programming_language: "C++"
category:             "C++"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Kyun?

Kabhi-kabhi hum chahte hain ki humare code mein se kuch characters ko hata diya jaaye jo humare pattern se match karte hain. Isse humare code ko clean and efficient banane mein madad milti hai.

## Kaise Karein?

Hamare paas do tareeko ke options hain character pattern matching ke liye: Regex (regular expressions) ya loop ke upyog se. Neeche diye gaye code blocks mein dono tareeko ka example diya gaya hai:

```C++
// Regex se character pattern hataayein

#include <iostream> 
#include <regex> 
using namespace std; 

int main() 
{ 
    string str = "Hello, World!"; 
    regex unwanted_characters ("[eo]"); //eo characters ko remove karne ke liye
    cout << regex_replace(str, unwanted_characters, "") << endl; 

    return 0; 
} 
```

```C++
// Loop se character pattern hataayein

#include <iostream> 
using namespace std; 

int main() 
{ 
    string str = "Hello, World!"; 
    string new_str = ""; // naya string create karein jaha hum characters ko store karenge
    for (int i = 0; i < str.length(); i++) { 
        if (str[i] != 'o' && str[i] != 'e') { // hum unwanted characters ko alag se nikaal dete hain
            new_str += str[i]; 
        } 
    } 
    cout << new_str << endl; 

    return 0; 
} 
```

## Gehri Jhaanki

Character pattern matching mein kuch cheezein yaad rakhein:
- Regex ka upyog karne se pehle `#include <regex>` jaroor karein
- Pattern ke saamne `\` backslash use karne se special characters ko match kar sakte hain (for example, \d for digits, \s for spaces)
- `regex_replace()` function ka use karke hum string ko replace kar sakte hain
- Loop mein hum `str.length()` ka upyog karke string ki length nikal sakte hain
- `new_str` ka use karke loop ke andar unwanted characters ko nikaal sakte hain 

## Dekhein Bhi

- [Regex tutorial in Hindi](https://www.youtube.com/watch?v=gGKxUTSD8eU) 
- [C++ loops tutorial in Hindi](https://www.geeksforgeeks.org/loops-in-cpp/)
- [C++ string operations](https://www.tutorialspoint.com/cplusplus/cpp_strings.htm)