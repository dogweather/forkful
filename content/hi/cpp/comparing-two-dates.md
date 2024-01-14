---
title:                "C++: दो तारीखों की तुलना करना"
simple_title:         "दो तारीखों की तुलना करना"
programming_language: "C++"
category:             "C++"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Kyun:
Programming mein kai baar humein do dates ko compare karna padta hai, jaise ki kisi event ki start date aur end date. Aise samay yeh zaroori hota hai ki hum sahi tareeke se dono dates ko compare karke sahi results prapt kar sake. Isliye, aaj hum baat karenge C++ mein do dates ko compare karne ke baare mein.

## Kaise:
Agar aap C++ sikh rahe hai ya phir kisi project mein kaam kar rahe hai jahan aapko do dates ko compare karna hai, toh aap sahi jagah aaye hai. C++ mein do dates ko compare karne ke liye hum kuch simple steps follow kar sakte hai:

Sabse pehle, humein do variables create karni hogi jo ki dates store karegi. Hum inn variables mein kisi bhi date format mein date ko store kar sakte hai, jaise ki `"DD/MM/YYYY"` ya `"MMMM DD, YYYY"`.

Fir hum `if` statement ka use karenge taaki humein pata chal sake ki konsi date badi hai aur uske according hum result print kar sake. Iske liye hum `>`, `>=`, `<` ya `<=` operators ka use kar sakte hai, jaise ki:

```C++
#include <iostream>
using namespace std;

int main() {
  // Variables to store dates
  string date1 = "04/10/2021";
  string date2 = "October 10, 2021";
  
  // Comparing dates
  if (date1 > date2) {
    cout << date1 << " is greater than " << date2 << endl;
  } else {
    cout << date2 << " is greater than " << date1 << endl;
  }
  
  return 0;
}
```

Is code mein humne pehle do variables create kiye, `date1` aur `date2`, aur phir humne `if` statement ka use kiya. Agar `date1` badi hai `date2` se, toh hum `date1` ko print karenge, nahi toh `date2` ko print karenge.

## Deep Dive:
Jaise humne coding example mein dekha, humein sahi result prapt karne ke liye kuch operators ka use karna padta hai. Yeh operators dates ko ASCII values mein convert karke compare karte hai. ASCII values mein, `0` se `9` tak ke numbers `48` se `57` ke beech hote hai, aur `A` se `Z` tak ke letters `65` se `90` ke beech hote hai.

Ise samajhne ke liye, ek example lete hai. Agar hum `"03/10/2021"` aur `"October 10, 2021"` dates ko compare kar rahe hai, toh `"03/10/2021"` ka ASCII value `48` se shuru hota hai, jabki `"October 10, 2021"` ka ASCII value `79` se shuru hota hai. Kyun ki `79` bada hai `48` se, isliye `"October 10, 2021"` badi date hai.

## Dekho Bhi:
- [C++ Comparison Operators](https://www.programiz.com/cpp-programming/comparison-operators)
- [ASCII Table](https://www.asciitable.com/)