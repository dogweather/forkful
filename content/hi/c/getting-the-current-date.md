---
title:                "C: वर्तमान तिथि प्राप्त करना"
simple_title:         "वर्तमान तिथि प्राप्त करना"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Kyon

Aaj kal, duniya bhaut fast paced hai aur hume har cheez ko instantly jaanna hota hai. Is liye, kaafi baar hume apni application mein current date ka use karna padta hai. Isse hum apne users ko accurate aur up to date information provide kar sakte hai.

## Kaise Karein

Current date prapt karne ke liye, hum C programming ka use kar sakte hai. C language mein "time.h" header file ko include karne se hum time-related functions ko access kar sakte hai. Isme se ek function "time()" hai jo hume current date aur time ka epoch format (seconds since January 1, 1970 UTC) return karta hai.

```
# include <stdio.h>
# include <time.h>

int main()
{
    time_t current_time;
    time(&current_time); // functions mein pass by reference hoti hai, isliye "&" use kiya hai

    printf("Epoch format: %ld\n", current_time);

    return 0;
}
```

Is code ke output ke roop mein, hum current date aur time ka epoch format dekh sakte hai. Lekin, is format ko padhna aur samajhna thoda mushkil ho sakta hai.

## Gehri Jhaank

Epoch format ka calculation ek specific time base par hota hai, jo ki January 1, 1970 UTC hai. Iske alawa, current date ko human-readable format mein display karne ke liye hum "struct tm" datatype ka use kar sakte hai. Iss structure mein hum specific components like day, month, year, hour, minute, second ko store kar sakte hai.

```
# include <stdio.h>
# include <time.h>

int main()
{
    time_t current_time;
    time(&current_time);

    // localtime() function current time ko apne local time zone ke hisaab se convert karta hai
    struct tm *local_time = localtime(&current_time);

    printf("Current date & time: %s", asctime(local_time));

    return 0;
}
```

Is code ke output mein hum current date aur time ko humare local time zone ke hisaab se dekh sakte hai.

## Dekhiye Bhi

Agar aapko C programming mein current date ko manipulate karne ke aur functions ke bare mein aur jaankari chahiye toh "time.h" header file ke official documentation ko refer kar sakte hai. Aur agar aapko specific project mein current date ka use karna hai, toh aapko "strftime()" function ko utilize karna padega. Is function mein hum date aur time ko humare desired format mein convert kar sakte hai.

- [time.h header file documentation](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [strftime() function in C](https://www.geeksforgeeks.org/strftime-function-in-c/)
- [Epoch time calculator](https://www.epochconverter.com/)