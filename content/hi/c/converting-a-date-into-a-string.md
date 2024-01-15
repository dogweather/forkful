---
title:                "तिथि को स्ट्रिंग में बदलना"
html_title:           "C: तिथि को स्ट्रिंग में बदलना"
simple_title:         "तिथि को स्ट्रिंग में बदलना"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Kyun
Iss article mein hum sikhenge ki kaise hum C mein ek date ko string mein convert kar sakte hain. Yeh kaam karna aapke programs ko visually appealing aur user-friendly banane mein madad karega.

## Kaise Karein
Din, mahina aur saal - yeh sab ek date mein hote hain. Lekin C mein hume date ko string mein convert karna padega taki hum usse display aur print kar sakein. Iske liye hum ek library function ka istemal karenge - strftime().

```
#include <stdio.h>
#include <time.h>

int main()
{
    // Ek time structure banayein
    time_t t = time(NULL);
    // Jo ki strftime() function ka ek argument hai
    // Bas ab desired format ka character array bana lein
    char str[100];
    // strftime() ka istemal karke date ko string mein convert karein
    strftime(str, sizeof(str), "%d-%m-%Y", localtime(&t));
    // Print karein
    printf("Aaj ka taareekh hai: %s", str);

    return 0;
}
```

Output: Aaj ka taareekh hai: 18-08-2021

Yahaan `%d` date ko, `%m` mahina ko aur `%Y` saal ko represent karta hai. Iske saath saath aap apni marzi ke hisaab se format ko customize kar sakte hain. Iske alawa, aap strftime() function ke aur bhi options explore kar sakte hain jaise ki weekday ya short month name ko display karne ke liye.

## Deep Dive
Iske alawa, hume ek aur important aspect ko dhyan mein rakhna hoga - timezone. By default, strftime() function UTC (Coordinated Universal Time) ka istemal karega. Lekin agar hume local time ko print karna hai, toh hum localtime() function ka istemal kar sakte hain. Isse humare code mein apne current time zone ka istemal hoga.

## Dekhen Bhi
Maine yahan kuch important resources listed ki hain, jinhein aap padhkar aur apne C coding skills ko improve karke date ko string mein convert karne mein aur bhi maahir ban sakte hain:

- [C strftime() function documentation](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [Different date and time format options in strftime() function](https://en.cppreference.com/w/c/chrono/strftime)
- [Converting local time to GMT/UTC in C](https://www.geeksforgeeks.org/converting-local-time-gmt-utc-c/)