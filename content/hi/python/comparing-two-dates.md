---
title:                "Python: दो तिथियों का तुलना करना"
programming_language: "Python"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/comparing-two-dates.md"
---

{{< edit_this_page >}}

**## Kyu**
Date comparison ek important concept hai jo logon ko python programming mein master hona chahiye. Ye hamein do tareeko se madad karta hai - pehle, ye hamein tareekh ka difference calculate karne mein madad karta hai aur dusra, ye humein date ke sath related logic aur algorithms samajhne mein madad karta hai.

**## Kaise Karein**
Pehle humein do dates ko input lena hoga:

```Python
date1 = input("Enter first date in format dd/mm/yyyy: ")
date2 = input("Enter second date in format dd/mm/yyyy: ")
```

Ab hum is input ko date format mein convert karenge aur phir ye dates ko compare karenge:

```Python
from datetime import datetime

date1 = datetime.strptime(date1, '%d/%m/%Y')
date2 = datetime.strptime(date2, '%d/%m/%Y')

if date1 > date2:
    print("{} is after {}".format(date1, date2))
elif date1 < date2:
    print("{} is before {}".format(date1, date2))
else:
    print("Both dates are same")
```

**## Gehree Jankari**

Date comparison mein kuch important chizon ka dhyan rakhna zaroori hai. Sabse pehle, input date strings ko sahi format mein convert karna zaroori hai. Iske liye, hum `strptime()` ka use karte hain jo input date ko datetime object mein convert karta hai.

Dusri baat, hum `datetime` module ko import karte hain jis se hum date aur time ke sath related kaam kar sakte hain. Iske alawa, hum `strftime()` ko use kar sakte hain jo datetime object ko specify format mein convert karta hai.

Date comparison mein "naive" aur "aware" dates ka concept bhi important hai. Naive date, timezone ke bina hota hai jabki aware date timezone ke sath hota hai. Iske alawa, leap years aur time zones ko bhi dhyan mein rakhna zaroori hai date comparison mein.

**## Dekhiye Bhi**
- [Python Dates and Times](https://www.w3schools.com/python/python_datetime.asp)
- [Comparing dates in Python](https://pynative.com/python-check-if-two-dates-are-equal/)
- [Python datetime module](https://docs.python.org/3/library/datetime.html)