---
title:                "तारीख को स्ट्रिंग में रूपांतरित करना"
html_title:           "Python: तारीख को स्ट्रिंग में रूपांतरित करना"
simple_title:         "तारीख को स्ट्रिंग में रूपांतरित करना"
programming_language: "Python"
category:             "Python"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Kyu
Koi bhi vyakti tithi ko ek string mein badal kar isse alag prakar se pradarshin kar sakta hai, jaise ki alag alag bhashao mein ya kisi specific format mein. Isse tithi ka upyog anek sthano par kiya ja sakta hai, jaise ki document mein, database mein ya kisi software mein.

## Kaise?
```Python
from datetime import date

today = date.today()

# dd/mm/YY
today_str = today.strftime("%d/%m/%Y")
print(today_str)

# Month name, Year
today_str = today.strftime("%B, %Y")
print(today_str)

# Full date with time
today_str = today.strftime("%A, %B %d, %Y at %I:%M %p")
print(today_str)
```

Output:
```
14/05/2021
May, 2021
Friday, May 14, 2021 at 07:00 PM
```

## Deep Dive
Python mein tithi ko string mein badalna kaafi aasan hai. Iske liye hum `strftime()` function ka upyog karte hai jo `datetime` module mein available hai. Is function mein hum tithi ke format ke anusaar `%` ke saath alag alag characters ka upyog kar sakte hai. In characters se hum tithi ko alag alag prakar se string mein badal sakte hai. Is prakar ki string formatting ka upyog anya bhi variables ke saath kiya ja sakta hai, jaise time, year, week, etc. Isse hum apne project ya code ke according tithi ko customize kar sakte hai.

## Dekhiye bhi
- [Python documentation on datetime](https://docs.python.org/3/library/datetime.html)
- [W3Schools tutorial on datetime in Python](https://www.w3schools.com/python/python_datetime.asp)
- [Real Python article on datetime formatting](https://realpython.com/python-datetime/#python-date-formatting)