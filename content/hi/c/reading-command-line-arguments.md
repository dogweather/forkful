---
title:                "कम्प्यूटर प्रोग्रामिंग पर कामांड लाइन आर्गुमेंट्स को पठन करना"
html_title:           "C: कम्प्यूटर प्रोग्रामिंग पर कामांड लाइन आर्गुमेंट्स को पठन करना"
simple_title:         "कम्प्यूटर प्रोग्रामिंग पर कामांड लाइन आर्गुमेंट्स को पठन करना"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Kyun

Kisi bhi programming language mein command line arguments bahut important hote hain. Ye humare programs ko user input se interact karne ka ek tarika hote hain. C programming language mein bhi command line arguments ka use bahut common hai, isliye is article mein hum jaanenge ki command line arguments kya hote hain aur unhe kaise padh sakte hain.

## Kaise

Command line arguments ko padhne ke liye hum `main()` function ke parameters ka use karte hain. `int argc` aur `char *argv[]` parameter hume program ke command line arguments ko hold karta hai. Yahan hum aapko ek simple example de rahe hai jismein hum command line arguments ko print karenge:

```C
#include <stdio.h>

int main(int argc, char *argv[]) {
    int i;
    for (i=1; i<argc; i++) {
        printf("Argument %d: %s\n", i, argv[i]);
    }
    return 0;
}
```

**Output:**

```
Argument 1: Hello
Argument 2: World
```

Humein baar baar command line arguments ko string mein convert karna padta hai kyunki `argv[]` array sirf strings ko store karta hai. Isliye agar hume kisi argument ko number ya character form mein use karna hai toh hume use `atoi()` aur `atof()` functions se convert karna padega, jaise ki yeh code snippet dikhaata hai:

```C
#include <stdio.h>
#include <stdlib.h>

int main(int argc, char *argv[]) {
    int num = atoi(argv[1]);
    float dec = atof(argv[2]);
    printf("Integer value: %d\n", num);
    printf("Decimal value: %f\n", dec);
    return 0;
}
```

**Command line input:**

```
./program_name 10 3.14
```

**Output:**

```
Integer value: 10
Decimal value: 3.140000
```

## Deep Dive

Agar hum baat karein toh command line arguments ko padhne ka kaam bahut simple hai. `argc` variable hume arguments ki total count deta hai aur `argv` array mein hum inko access kar sakte hain. Lekin kya aap jaante hain ki hum kisi bhi program mein command line arguments ko modify bhi kar sakte hain? Haan, hum iske liye `int main()` function ke jagah `int main(int argc, char *argv[], char *envp[])` declare kar sakte hain. Yahan `envp[]` parameter hume environment variables ko hold karta hai aur hum inhe bhi access aur modify kar sakte hain.

## See Also

Is article mein humne sirf basic command line argument handling ke baare mein baat ki hai. Agar aapko aur zyada information chahiye toh aap neeche diye gaye resources ko check kar sakte hain.

- [The Linux Documentation Project - Command Line Arguments in C](https://tldp.org/LDP/lpg/node11.html)
- [GeeksforGeeks - Command Line Arguments in C/C++](https://www.geeksforgeeks.org/command-line-arguments-in-c-cpp/)