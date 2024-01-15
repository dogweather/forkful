---
title:                "दो दिनांकों की तुलना"
html_title:           "Python: दो दिनांकों की तुलना"
simple_title:         "दो दिनांकों की तुलना"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

## Kyu

Kya aap kabhi do tareekho ka tulna karke dekha hai? Yadi aapne kiya hai, to shayad aapko pata hoga ki ye ek aasan lekin mahatvapurn kaam hai. Python me tarikhon ka tulna karne ke liye bhi kuchh aasan tareeke hai. Is lekh mein hum dekhenge ki kaise hum do tareekhon ko tulna kar sakte hain aur kya mahatvapurn baatein hain jo is samasya ke samadhan me madad karengi.

## Kaise Karein

```Python
# Pehle hum datetime module ko import karenge
import datetime

# Fir, do tareekhon ka hamesha ke liye saath me rakhkar ek datetime object banayenge
date1 = datetime.datetime(2020, 8, 9)
date2 = datetime.datetime(2021, 8, 9)

# Ab ham compare() method ka upyog karke in dono tareekhon ka tulna kar sakte hain
result = date1.compare(date2)

# Ab ham output ko dekhte hain
print(result)

# Iska output humein "1 Year, 0 Months, 0 Days" dega. Yani ki dono tareekhon me ek saal ka antar hai.
```

Python me, hum datetime module ka upyog karke aasani se do tareekhon ka tulna kar sakte hain. Iske alawa, hum strftime() method ka bhi istemal karke do tareekhon ke beech kitne din, mahine aur varsh ka antar hai, isse bhi pata laga sakte hain. Iske liye, hum %D (Days), %M (Months) aur %Y (Years) format ka upyog kar sakte hain.

```Python
# Pehle hum dono tareekhon ko ek string me convert karenge
date1 = "2020-08-09"
date2 = "2021-08-09"

# Ab hum datetime.strptime() method ka upyog karke in tareekhon ko ek datetime object me convert karenge
date1 = datetime.strptime(date1, "%Y-%m-%d")
date2 = datetime.strptime(date2, "%Y-%m-%d")

# Ab hum strftime() method ka upyog karke dono tareekhon ke beech ka antar pata laga sakte hain
result = date2.strftime("%Y Years, %M Months, %D Days") % (date2 - date1)

# Ab ham output ko dekhte hain
print(result)

# Iska output humein "1 Year, 0 Months, 0 Days" dega, jo ki hamare pehle example ke output se bilkul same hai.
```

## Gehri Jankari

Python me tarikhon ka tulna karna kaafi aasan hai. Lekin kuchh chhoti chijon ko dhyaan me rakhna jaruri hai. Python me tareekhon ka compare() method humesha keval days, seconds aur microseconds ka antar hi pata karta hai. Yadi aap saal, mahine aur dinon ka antar chahte hain, to aapko strftime() method ka upyog karna hoga.

Ek aur baat jo dhyaan me rakhni hai, wo hai leap years. Python me leap years ko handle karne ke liye calendar module ka upyog kiya ja sakta hai. Iske alawa, hum ye bhi jaan sakte hain ki kisi saal me kitne din hain, ya kisi mahine me kitne din hain, iske liye bhi hum calendar module ka upyog kar sakte hain.

Isi tarah, Python me tareekhon ka tulna karne ke liye aur bhi kai madadgar modules hain, jaise ki arrow, dateutil, dateparser, etc. Inka upyog karke bhi hum do tareekhon ke beech ka antar pata laga sakte hain.

## Dekhein Bhi

Agar aapko Python programming ke bare me aur bhi behtar jankari chahiye, to aap ye links dekh sakte hain:

- [Python Tutorials in Hindi](https://www.geeksforgeeks.org/python-programming-language/)
- [Official Python