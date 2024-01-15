---
title:                "स्टैंडर्ड इरर में लिखना"
html_title:           "C: स्टैंडर्ड इरर में लिखना"
simple_title:         "स्टैंडर्ड इरर में लिखना"
programming_language: "C"
category:             "C"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Kyun
Standard error mein likhne ka kya faayda hai? Ye sawaal aksar naye C programmers ke mann mein aata hai. Lekin standard error mein likhna bahut zaroori hai kyunki ye humari debugging aur error handling capability ko badhata hai. Isse hum errors ko track kar sakte hain aur apne code ko sahi tarike se troubleshoot kar sakte hain.

## Kaise Karein
Sabse pehle, hume `stdio.h` header file ko apne code mein include karna hoga. Phir hum `fprintf()` function ka use karenge jo standard error mein likhne ke liye hai. Iske baad hum desired error message ko likh sakte hain. Yeha ek simple example hai:

```
#include <stdio.h>

int main()
{
  fprintf(stderr, "Yeh ek error message hai!");
  
  return 0;
}
```
**Output:**
```
Yeh ek error message hai!
```

Is tarah se hum apne code mein multiple `fprintf()` statements bhi likh sakte hain aur apne errors ko alag-alag lines mein display kar sakte hain.

## Deep Dive
Standard error ka use sirf error messages likhne ke liye hi nahi kiya ja sakta hai. Hum isse warnings aur debugging messages bhi likh sakte hain taki humare code ki flow aur performance ka pata lag sake. Iske sath hi, hum standard error ka use apni command-line utilities mein bhi kar sakte hain jaha hume output ke sath-sath errors aur warnings bhi display karne hai.

Ek aur important technique hai standard error ka use kar ke hum apne code mein log messages likh sakte hain. Ye hume debugging mein bahut help karta hai kyunki hume pata lagta hai ki code kaunse part mein zyada time le raha hai aur kis section mein errors ho rahe hain. Isse hum apne code ki performance ko improve kar sakte hain.

## Dekhein Bhi
- [Original article on writing to standard error](https://www.geeksforgeeks.org/write-to-standard-error/)
- [Difference between standard error and standard output](https://www.geeksforgeeks.org/what-is-the-difference-between-stderr-and-stdout/)
- [Debugging techniques in C](https://www.geeksforgeeks.org/debugging-and-its-types/)

Is article se hum samajh gaye hain ki standard error mein likhna kyun aur kaise zaroori hai. Ye debugging aur error handling capability ko badhata hai aur hume apne code ko sahi tarike se troubleshoot karne mein help karta hai. Humesha apne code mein standard error ka use karna na bhulein!