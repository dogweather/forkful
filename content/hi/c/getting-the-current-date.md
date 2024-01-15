---
title:                "तारीख को प्राप्त करना"
html_title:           "C: तारीख को प्राप्त करना"
simple_title:         "तारीख को प्राप्त करना"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Kyu

Aapne kabhi socha hai ki aap ek program banate hai aur wo program hamesha current date ke according chalta rahe? Ya phir aapko kisi bhi tarah ki transaction pe current date aur time ka record rakhna ho? Ya fir aap simply apne program mai kisi bhi tarah ki date aur time se related feature add karna chahte hai? Ye sab cheezein karne ke liye hume current date aur time ki zarurat padhti hai. Isi liye hume C programming mai current date ko acquire karna aana chahiye.

## Kaise

Agar aap C programming seekh rahe hai ya phir aapke paas pehle se C ki jaankari hai to aapko ye baat pata hogi ki C programming mai kisi bhi tarah ka date aur time ka record rakhna current date aur time ko acquire karna se shuru hota hai. Iske liye hume pata hona chahiye ki hum current date aur time ko kis tarah se acquire kar sakte hai.

Ek simple sa example hai jo current date aur time ko acquire karne mai humari madad karega.

```C
#include <stdio.h>
#include <time.h>

int main()
{
    time_t t = time(NULL);
    struct tm *now = localtime(&t);

    printf("Current Date and Time: %d/%d/%d %d:%d:%d",
            now->tm_mday, now->tm_mon+1, now->tm_year+1900,
            now->tm_hour, now->tm_min, now->tm_sec);

    return 0;
}
```

Output:

```
Current Date and Time: 19/9/2021 11:30:44
```

Is example mai humne `time.h` library ka use kiya hai jo current date aur time ko acquire karne mai humari madad karta hai. `time()` function current date aur time ko seconds mai return karta hai. Isliye humne `time_t` variable mai iska value assign kardiya. Phir humne `localtime()` function ka use kiya hai jo current date aur time ko local timezone mai convert karta hai. Iske baad hum ek `struct tm` variable mai current date aur time ki details ko store karke uska address `localtime()` function mai pass kiya hai. Phir humne `printf()` function ka use kiya hai jo humara output dega current date aur time ke format mai.

Humne yaha `%d` ka use kiya hai jiski madad se hum integer values ko print kar sakte hai. `%d`, `%m`, `%y` aur `%Y` current date ki day, month, year aur century ko represent karte hai. Similarly, `%H`, `%M` aur `%S` current time ke hours, minutes, aur seconds ko represent karte hai.

## Deep Dive

Haal hi mai release hui C11 version mai `time.h` library ka use karke current date aur time ko acquire karna aur bhi easy ho gaya hai. Is version mai `struct tm` ke saath aur bhi functions jaise ki `tm_year` aur `tm_mon` ko operate karne ke liye `iasctime_s` function add kiya gaya hai. Isse hume date aur time ko human readable format mai display karna aur bhi aasan ho gaya hai.

Iske alawa `chrono.h` library ka use karke bhi hum current date aur time ko acquire kar sakte hai. Is library mai hume `year_month_day` aur `year_month_weekday` jaise classes milte hai jo hume date ki specific details provide karte hai. Isiliye C11 version mai current date aur time ko acquire karna aur bhi efficient ho gaya hai.

## Dekhiye Bhi

- [C programming tutorials](https://www.programiz.com/c-programming)
- [C11 standard documentation](https://www.iso.org/standard/69318.html)
- [Chrono library documentation](https://en.cppreference.com/w/cpp/chrono)