---
title:                "Language: Hindi Translation: स्ट्रिंग को कैपिटलाइज करना"
html_title:           "C: Language: Hindi Translation: स्ट्रिंग को कैपिटलाइज करना"
simple_title:         "Language: Hindi Translation: स्ट्रिंग को कैपिटलाइज करना"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## Kyun
Aapne shayad suna hoga ki C programming language mein string ko upper case mein tabdil karne ke liye "toupper()" function ka upyog kiya jaata hai. Lekin kya aapko pata hai ki kya ye function sirf ek character ko upper case mein badal sakta hai? Agar aap pure string ko capital letters mein badalna chahte hai toh aapko kya karna hoga? Isiliye hum aapko aaj batayenge ki string ko capital letters mein convert karna kyun zaruri hai aur iske kya fayde hai.

## Kaise Karein
Agar aapko kisi bhi string mein se specific characters ko capital letters mein convert karna hai toh aap "toupper()" function ka istemal kar sakte hai. Is function mein aapko woh character dena hoga jisko aap upper case mein badalna chahte hai. Lekin agar aap pure string ko upper case mein badalna chahte hai toh aapko ek loop mein har character ko upper case mein convert karna hoga. Iske liye hum ek simple code example de rahe hai:

```C
#include<stdio.h>
#include<ctype.h>

int main() {
   char str[20] = "hello world";
   int i;

   for (i = 0; str[i]!='\0'; i++) {
      str[i] = toupper(str[i]);
   }

   printf("%s", str);
   return 0;
}
```
Is code mein humne "toupper()" function ka istemal kiya hai jo hume pure string ko upper case mein convert karne mein madad karta hai. Isko compile aur run karne par aapko output "HELLO WORLD" milna chahiye. Is tarah aap kisi bhi string ko upper case mein badal sakte hai.

## Gehri Pariweshik
Ab hum dekhenge ki string ko capital letters mein convert karna kaise kaam karta hai. Sabse pehle humne ek character array banaya jisme hum "hello world" string store kiye hai. Iske baad humne "for" loop ka upyog kiya hai jisme humne string ke har character ko upper case mein convert kiya hai. Yeh loop ek ek character ko upper case mein badalta hai aur ise string mein store kar deta hai. Is tarah string ke sabhi characters upper case ho jate hai aur aap "printf()" function ka upyog karke output ko dekh sakte hai. Iss tarah aap kisi bhi string ko upper case mein badalkar use aur aasan aur readable bana sakte hai.

## See Also
- [toupper() function in C](https://www.geeksforgeeks.org/c-programming-language-toupper-function/)
- [Understanding character arrays in C](https://www.programiz.com/c-programming/c-strings)

Is article mein humne dekha ki kaise hum string ko capital letters mein convert kar sakte hai. Iska upyog karke aap apne code ko aur bhi behtar aur readable bana sakte hai. Humne aapko C programming language mein string ko capital letters mein convert karne ke liye ek simple aur effective method bataya hai. Hum aasha karte hai ki aapko yeh article pasand aaya hoga aur aap iska upyog apne code mein karke dekhenge. Happy coding!