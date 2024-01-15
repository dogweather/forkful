---
title:                "टेक्स्ट फ़ाइल लिखना"
html_title:           "C: टेक्स्ट फ़ाइल लिखना"
simple_title:         "टेक्स्ट फ़ाइल लिखना"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Kyun
Agar aap C programming language ke sath kaam kar rahe hain toh shayad aapne kabhi na kabhi text files ke jaal ko experience kiya hoga. Text files hamare code ke liye ek essential part hote hain jisko ham user input, output ya storage ke liye istemal karte hain. Is article mein hum baat karenge ki kis tarah se aap C mein text file ko likh sakte hain.

## Kaise
Sabse pehle hum file handle ko declare karte hain. File handle ek special variable hota hai jiska istemal file ke data ko read aur write karne ke liye kiya jata hai. Iske baad hum `fopen()` function ka istemal karte hain jiska syntax `FILE *fopen(const char *filename, const char *mode)` hota hai. Is function mein filename aur mode specify kiya jata hai. Mode ko "w" assign karne se ek file ko write karne ka permission milta hai. Uske baad hum `fprintf()` function ka istemal karte hain jiska syntax `int fprintf(FILE *f, const char *format, ...)` hota hai. Yeh function text file mein data likhne ke liye use kiya jata hai. Iske baad hum file ko close kar dete hain `fclose()` function ka istemal karke.

```C
#include <stdio.h>
int main() {
  FILE *fp;
  fp = fopen("example.txt", "w");
  fprintf(fp, "Hello, world!");
  fclose(fp);
  return 0;
}
```

Jab aap is code ko run karenge, toh aapko current working directory mein "example.txt" naam ka ek text file milega jisme "Hello, world!" likha hua hoga.

## Deep Dive
Is section mein hum text file ke likhne ke process ko aur detail mein jaayenge. Sabse pehle `fopen()` function ke baare mein baat karte hain. Is function mein humne `const char *filename` aur `const char *mode` use kiya. `const` ka matlab hai ki hum is variable mein value assign nahi kar sakte hain. `char` data type string ko represent karta hai aur `*` operator ka istemal us variable ki memory address ko point karne ke liye kiya jata hai. `fopen()` function se hume ek pointer return hota hai jo hum file handle mein assign kar sakte hain.

Agar aap text file mein multiple lines likhna chahte hain, toh `fprintf()` function mein `"\n"` ka istemal karke line break kar sakte hain. Iske alawa hum `fprintf()` function mein variables, integers, floats, characters, etc. bhi print kar sakte hain. `fclose()` function ka istemal file ko address ko release karne ke liye kiya jata hai.

## See Also
- [C File Input/Output](https://www.programiz.com/c-programming/c-file-input-output)
- [File Handling in C](https://www.tutorialspoint.com/cprogramming/c_file_io.htm)
- [C File Handling Tutorial](https://www.youtube.com/watch?v=1PZ7fVJHwN0)