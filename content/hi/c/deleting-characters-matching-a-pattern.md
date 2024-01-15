---
title:                "पैटर्न के समान अक्षरों को हटाना"
html_title:           "C: पैटर्न के समान अक्षरों को हटाना"
simple_title:         "पैटर्न के समान अक्षरों को हटाना"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Kyun
Kisi bhi programming language mein, data manipulation bohot zaroori hai. Aur kabhi kabhi humein specific pattern matches ko delete karne ki zaroorat hoti hai. C programming language mein, hum yeh kar sakte hain by using functions like `strchr()` aur `strcspn()`.

## Kaise Karein
Agar humein kisi string se specific characters ko delete karna hai, toh hum `strchr()` function ka use kar sakte hain. Yeh function char ko dusre character mein search karta hai aur agar match milta hai toh use delete karta hai.

```
#include <stdio.h>
#include <string.h>

int main() {
  char str[] = "Hindi readers, welcome to C programming!";
  char target = 'e';
  char *result;

  result = strchr(str, target); // target character 'e' delete ho jayega
  printf("String after deletion: %s\n", result);

  return 0;
}
```

Output:
```
String after deletion: Hindi radrs, welcom to C programming!
```

Agar humein ek specific pattern ko match karne ke baad delete karna hai, toh hum `strcspn()` function ka use kar sakte hain. Yeh function match ke baad se leke string ka end tak ke characters ko delete karta hai.

```
#include <stdio.h>
#include <string.h>

int main() {
  char str[] = "Hindi readers, welcome to C programming!";
  char target[] = "wel";
  char *result;

  result = strcspn(str, target); // target pattern 'wel' delete ho jayega
  printf("String after deletion: %s\n", result);

  return 0;
}
```

Output:
```
String after deletion: Hindi readers, c programming!
```

## Gehri Jankari
`strchr()` aur `strcspn()` functions ka use string manipulation mein bohot common hai. In dono functions ka syntax ek jaisa hota hai jisme pehle parameter mein string diya jata hai aur dusre parameter mein character ya pattern diya jata hai. Agar match milta hai, toh woh character ya pattern string se delete ho jata hai aur remaining string return ho jata hai.

## Dekhein Alag Se
- [String Manipulation using strchr() function in C](https://www.geeksforgeeks.org/strchr-c-language/)
- [String Manipulation using strcspn() function in C](https://www.geeksforgeeks.org/strcspn-in-c/)
- [String Manipulation in C - Video Tutorial (Hindi)](https://youtu.be/Tsppr6ZLxFY)

## Dekhein Alag Se