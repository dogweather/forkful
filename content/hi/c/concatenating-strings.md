---
title:                "स्ट्रिंग्स को जोड़ना"
html_title:           "C: स्ट्रिंग्स को जोड़ना"
simple_title:         "स्ट्रिंग्स को जोड़ना"
programming_language: "C"
category:             "C"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/concatenating-strings.md"
---

{{< edit_this_page >}}

##Kyunki

Jab hum C programming mein string concatenation karte hain, matlab hum alag-alag strings ko milakar ek badi string bana rahe hain. Isse hum apne program mein ek dynamic aur versatile approach implement kar sakte hain.

##Kaise Karein

String concatenation ke liye hum "strcat()" function ka use karte hain. Iske liye hum pehle "string.h" header file ko include kar lete hain.

```
#include <stdio.h>
#include <string.h>

int main(void) {
  char str1[30] = "Hello, ";
  char str2[] = "World!";
  
  strcat(str1, str2);
  
  printf("%s", str1);
  // Output: Hello, World!
  return 0;
}
```

Ismain, humne "str1" string variable mein "Hello, " string assign kiya hai aur "str2" string variable mein "World!" string. Humne "strcat()" function se "str2" string variable ko "str1" mein concatenate kiya hai aur fir output ke liye "printf()" function ka use kiya hai.

##Gehrai Mein Jaanein

String concatenation ke liye do methods hote hain - "strcat()" aur "+=" operator. Dono methods mein hum ek string variable mein dusre string variable ko concatenate karte hain. Lekin "+" operator ka use karne ke liye humein pehle ek empty string create karna padta hai.

```
#include <stdio.h>
#include <string.h>

int main(void) {
  char str1[30] = "Hello, ";
  char str2[] = "World!";
  
  strcat(str1, str2);
  // OR str1 += str2;
  
  printf("%s", str1);
  // Output: Hello, World!
  return 0;
}
```

"strcat()" ka use karte samay humein pehle "str1" mein concatenate karna hai aur fir "str1" ko hi output ke liye use karna hai. Lekin "+" operator ka use karte samay hum ek empty string bana kar isi mein concatenate karte hain aur fir output ke liye "str1" variable ka use karte hain.

##Dekhein Bhi

Agar aap aur functions aur operators ke bare mein jaanna chahte hain jo string concatenation mein use kiye jaate hain, toh is link par jayein: [String Concatenation in C](https://www.programiz.com/c-programming/library-function/string.h/strcat).

Iske alawa, aap ye bhi jaan sakte hain ki C programming mein strings kaise kaam karte hain: [Strings in C](https://www.programiz.com/c-programming/c-strings).