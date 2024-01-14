---
title:    "C: दो तारीखों की तुलना करना"
keywords: ["C"]
---

{{< edit_this_page >}}

## Kyon

Aaj ke samay mein dino aur tarikhon ka apne rojana jeevan mein mehatva hai. Kabhi kabhi hume do alag-alag tarikhon ka tulna karna hota hai, jaise ki kisi event ki shuruat aur antar ya apne janm din aur current date ka antar. Aise samay mein, hume dono dates ka samanta ya antar jaanna jaroori hai. Isliye, do dino ko compare karne ke liye ek aasan aur sahi tarika sikhna zaroori hai.

## Kaise Karein

Coding mein dates ko compare karne ke liye, hume pahle se jana hua "struct tm" ka use karna hota hai. Yeh ek date aur time ko store karne ke liye pre-defined data structure hai. Chaliye, ek simple example ke through dekhte hai kaise hum struct tm ka use kar sakte hai:

```C
#include <stdio.h>
#include <time.h>

int main()
{
    struct tm start_date , end_date;

    // Start Date
    start_date.tm_year = 2020;
    start_date.tm_mon = 8;
    start_date.tm_mday = 12;

    // End Date
    end_date.tm_year = 2021;
    end_date.tm_mon = 8;
    end_date.tm_mday = 12;

    // Comparing dates
    if (difftime(mktime(&end_date), mktime(&start_date)) > 0)
    {
        printf("Start Date is before End Date\n");
    }
    else if (difftime(mktime(&end_date), mktime(&start_date)) < 0)
    {
        printf("End Date is before Start Date\n");
    }
    else
    {
        printf("Both dates are equal\n");
    }

    return 0;
}
```

Output:
```
Start Date is before End Date
```

Jaise ki aap dekh sakte hai, humne pehle start date aur end date ko struct tm ke variable mein store kiya. Fir difftime() function ka use karke dono dates ka antar calculate kiya. Agar diffrence positive aata hai, toh start date end date se pehle hai aur agar negative aata hai, toh end date start date se pehle hai.

Is tarah se, aap kisi bhi date ko compare kar sakte hai. Refer karein is link [struct tm](https://www.tutorialspoint.com/c_standard_library/c_function_mktime.htm) aur [difftime()](https://www.tutorialspoint.com/c_standard_library/c_function_difftime.htm) ke liye aur jaaniye iske aur bhi functions.

## Gehri Khurak

Dates ko compare karna code likhte waqt kaafi important hota hai. Aapko pehle se start date aur end date ka pata hona zaroori hai varna aap galat output receive kar sakte hai. Saath hi, sahi format mein date ko store karna bhi zaroori hai. Isliye, dhyan se code likhe aur apne code ko test karein alag-alag inputs ke saath.

## Dekhiye Bhi

Agar aapko C programming aur dates ko compare karne mein aur bhi jankaari chahiye toh dekhiye is link [Comparison of dates and times in C](https://www.codingunit.com/comparison-of-dates-and-times-in-c) ko. Ismein aur bhi saare concepts aur techniques bataye gaye hai. Happy coding!