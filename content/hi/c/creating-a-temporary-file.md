---
title:                "एक अस्थायी फ़ाइल बनाना"
html_title:           "C: एक अस्थायी फ़ाइल बनाना"
simple_title:         "एक अस्थायी फ़ाइल बनाना"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## Kyun
Temporary file banana ke peeche koi khaas wajah hai. Kabhi kabhi hamare program mein temporary data store karna zaroori hota hai, jaise ki temporary results ko save karna ya temporary input data ko read karna. Temporary file banane se hum apne program ko flexible aur efficient banate hai.

## Kaise Karein
Temporary file banane ke liye, hume sabse pehle "stdio.h" header file ko include karna hoga. Uske baad, hum `fopen` function ka use karke temporary file ko create kar sakte hai. Iske badle hume ek unique file name generate karna hoga, jise `tmpnam()` function ke zariye kar sakte hai. Finally, hume temporary file ko delete karna zaroori hai, iske liye hum `remove()` function ka use kar sakte hai. Niche diye gaye code snippet mein ek example hai:

```C
#include<stdio.h>

int main() {
  FILE *pFile;
  char tempFile[50];

  // Temporary file create karna
  pFile = fopen("temp.txt", "w+");

  // Unique file name generate karna
  tmpnam(tempFile);

  // Temporary file ko delete karna
  remove(tempFile);

  return 0;
}
```

## Deep Dive
Temporary files, ya temp files, humare program mein temporary data storage ke liye bahut zaroori hote hai. In files ko create karne ka sabse aasan tarika `fopen()` function ka use karna hai. Jab hum temporary file create karte hai, toh hume ek unique file name assign karna zaroori hai, taaki humari existing files ke saath conflict na ho. Iske liye `tmpnam()` function zariye hum unique file names generate kar sakte hai. Temporary file delete karne ke liye, hume `remove()` function ka use karna hai. Isse humari temporary file system se remove ho jayegi.

## Dekhiye Bhi
- [C Programming Tutorials Series by Programiz](https://www.programiz.com/c-programming)
- [Temporary Files in C by GeeksforGeeks](https://www.geeksforgeeks.org/tmpnam-example-c/)
- [The Importance of Temporary Files in Programming by Javatpoint](https://www.javatpoint.com/temporary-files-in-programming)