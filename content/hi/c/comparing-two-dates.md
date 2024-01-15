---
title:                "दो तारीखों की तुलना"
html_title:           "C: दो तारीखों की तुलना"
simple_title:         "दो तारीखों की तुलना"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Kyu
Kisi bhi developer ke liye do tareekhon ki tulana karna ek aam samasya hai. Is article mein hum dekhenge ki kaise date ke do vyakti alag-alag tarikon se compare kiya ja sakta hai.

## Kaise
Date comparison karne ke liye, hum `time.h` library mein available `time` structure ka istemaal karenge. Iske liye, hum do `time` structures banayenge, jismein hum date ki details ko store karenge. Fir hum `difftime()` function ka use karke dono dates ke beech mein difference calculate karenge. Yeh difference humare code ka output hoga. 

```C
#include <stdio.h>
#include <time.h>

int main(){
    // Creating two time structures
    time_t d1, d2;
    // Storing dates in structures
    struct tm t1 = { .tm_year = 2020, .tm_mon = 05, .tm_mday = 21 };
    struct tm t2 = { .tm_year = 2020, .tm_mon = 07, .tm_mday = 18 };
    // Converting structures to time variables
    d1 = mktime(&t1);
    d2 = mktime(&t2);
    // Calculating difference
    double result = difftime(d2, d1);
    // Outputing difference
    printf("Difference between dates is %.2f days.", result);
}
```

```
Output:
Difference between dates is 58.00 days.
```

## Deep Dive
Humne structure banane ke liye `tm` structure ka istemaal kiya kyunki ismein hum date ki details, jaise year, month, day, hour, minutes, seconds, store kar sakte hain. Iss structure ko `time.h` library mein `struct tm` naam se declare kiya gaya hai. `difftime()` function hamare liye dates ke beech mein difference calculate karta hai aur iska output seconds mein deta hai. Isiliye humne use `%f` format specifier ke saath output karvaya aur use double data type mein store kiya.

## Dekhna Bhi
- [C date and time functions](https://www.programiz.com/c-programming/c-date-time)
- [Understanding tm structure in C](https://www.geeksforgeeks.org/time-structure-in-c/)