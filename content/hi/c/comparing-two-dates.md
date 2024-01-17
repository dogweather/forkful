---
title:                "दो तारीखों का तुलना"
html_title:           "C: दो तारीखों का तुलना"
simple_title:         "दो तारीखों का तुलना"
programming_language: "C"
category:             "C"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Kya & Kyon?

Tareekh aur samay ke do tarikhon ko tulna karne ka matlab hai ki programmer do tarikhon ko mukhay roop se compare karte hain. Yeh aksar prashn hai ki kyoon programmers yeh karte hain. Ek simple sa jawab hai ki hume do tarikhon ke beech antar ya similarities dhundhna hota hai. Isse hume aage ki coding mein madad mil sakti hai ya hume kisi specific kaam ke liye tarikhon ka pata lagana ho sakta hai. Isliye, tarikhon ko compare karna programming mein ek important task hai.

## Kaise Karein?

```C
#include <stdio.h>
#include <time.h>

int main()
{
  struct tm date1 = {0}; // pehli tarikh
  struct tm date2 = {0}; // dusri tarikh
  
  // tarikhon ko set karein
  date1.tm_year = 2020 - 1900; // 2020 saal
  date1.tm_mon = 10 - 1; // October (1 se kam dena hota hai)
  date1.tm_mday = 20; // 20th tarikh
  date2.tm_year = 2020 - 1900;
  date2.tm_mon = 11 - 1;
  date2.tm_mday = 25;
  
  // tarikhon ko compare karein
  if (mktime(&date1) < mktime(&date2))
  {
    printf("Tarikh 1 pehle aati hai.");
  }
  else if (mktime(&date1) > mktime(&date2))
  {
    printf("Tarikh 2 pehle aati hai.");
  }
  else
  {
    printf("Dono tarikhon mein koi antar nahi hai.");
  }
  return 0;
}
```

```C
Output:
Tarikh 1 pehle aati hai.
```

## Gehri Khurafat

Do tarikhon ko compare karne ka sawaal pehle bhi utha hai. Yeh ek purani problem hai aur bahut saare methods hain jaise ki join aur merge. Par ab yeh problem bahut hi asaan hai kyonki libraries jaise ki ```time.h``` has milti hai. Aur agar aap advanced level par coding karte hain toh aap khud bhi kuch functions likh sakte hain jo tarikhon ko compare karein.

## Aage Padhein

Agar aapko tarikhon ke functions aur algorithms se compare karna accha lagta hai toh aap inke baarein mein aur padh sakte hain: [Date and Time Functions in C](https://www.tutorialspoint.com/c_standard_library/time_h.htm)