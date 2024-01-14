---
title:    "C++: पैटर्न के मिलते हुए चरित्रों को हटाना"
keywords: ["C++"]
---

{{< edit_this_page >}}

## Kyun:
Kisi bhi programmer ke liye, samay ke saath saath unke codebase mein kayi saare changes aur updates hote rehte hain. Inme se ek common issue hota hai ki, kuch characters ka pattern match hona aur unhe delete karna. Lekin ye kaam baar baar manually karna kafi tedious aur time consuming ho sakta hai. Isi liye, hum C++ mein ek program likhenge jo kisi bhi pattern match karne aur use delete karne ko automated aur efficient banayega.

## Kaise Karein:
Sabse pehle, hum C++ ka ek basic program likhenge jo input lenge aur usme se user dwara diye gaye pattern ko dhundhkar use delete kar dega. Iske liye, hum `find()` aur `erase()` functions ka istemal karenge. Neeche diye gaye code block mein, humne ek string liya aur usme se sabse pehle "a" character ko delete kar diya.

```C++
#include <iostream>
#include <string>

using namespace std;

int main(){
    string str = "Programming";
    str.erase(str.find("a"), 1);
    cout << str;
    return 0;
}

// Output: Progmming
```

Agar hume isi tarah se dusre characters bhi delete karne hote hain, toh hum `replace()` function ka bhi istemal kar sakte hain. Ye function string mein se ek character ko dusre ke saath replace karta hai. Neeche diye gaye code block mein, humne string se pehle "a" character ko "o" character ke saath replace kiya.

```C++
#include <iostream>
#include <string>

using namespace std;

int main(){
    string str = "Programming";
    str.replace(str.find("a"), 1, "o");
    cout << str;
    return 0;
}

// Output: Proogromming
```

## Gahrai Mein Jaayein:
Iss tarah, humne C++ mein koi bhi character pattern ko delete karna sikh liya. Lekin, agar hum kisi specific character se zyada characters ko delete karna chahte hain, ya phir kuch complex patterns ko handle karna chahte hain, toh hum Regular Expressions ka istemal kar sakte hain. Ye hume pattern matching aur manipulations mein zyada flexibility dete hain. Ye C++ ke `regex` library mein available hote hain.

## Dekhein Bhi:
- [`find()` function in C++](https://www.geeksforgeeks.org/c-string-find-function/)
- [`erase()` function in C++](https://www.geeksforgeeks.org/c-string-erase-function/)
- [`replace()` function in C++](https://www.geeksforgeeks.org/c-string-replace-function/)
- [Regular Expressions in C++](https://www.geeksforgeeks.org/the-c-standard-library-regex/)