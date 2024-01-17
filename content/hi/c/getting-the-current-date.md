---
title:                "वर्तमान दिनांक प्राप्त करना"
html_title:           "C: वर्तमान दिनांक प्राप्त करना"
simple_title:         "वर्तमान दिनांक प्राप्त करना"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

# Kya aur Kyun?
"Current date" ya "today's date" ek common programming concept hai jo aaj ke date ko identify aur use karne ko allow karta hai. Ye kafi useful hai jab aap apne program mein time-based calculations, event tracking, data organization ya user interface updates jaise tasks karna chahte hain. Isse aap samay pe data ka sahi use kar sakte hain aur apne program ke actions ko current aur accurate rakh sakte hain.

# Kaise:
```C
#include <time.h>
#include <stdio.h>

int main() {
    time_t current_time;
    struct tm* local_time;

    current_time = time(NULL);
    local_time = localtime(&current_time);

    printf("Aaj ka date: %d/%d/%d", local_time->tm_mon+1, local_time->tm_mday, local_time->tm_year+1900);
    return 0;
}
```

Output: Aaj ka date: 8/13/2021

# Grahan:
"Current date" concept programming world mein kafi purana hai. Pehle jab computers mein date ko track karna necessary nahi tha, tab bhi kuch languages mein iske built-in functions hote the. Aaj bhi kuch languages mein date ko manipulate karte samay yehi concept use kiya jata hai.

Agar aapko date ko change karna hai ya fir kisi specific date ko use karna hai to aap current date ke badle manually date ko initialize kar sakte hain. Iske liye aap "struct tm" ko use kar ke date, month aur year ko separately assign kar sakte hain.

# Dekhen Bhi:
- [time.h library documentation](https://www.tutorialspoint.com/c_standard_library/time_h.htm)
- [Current date explanation in Hindi](https://www.studytonight.com/c/programming/get-current-date-in-c#:~:text=Current%20date%20in%20C%20languages%20is%20a%20very,is%20an%20important%20concept%20in%20every%20programming%20language.)
- [Date and time manipulation in C](https://www.geeksforgeeks.org/date-time-manipulation-handling-date-time-c/)