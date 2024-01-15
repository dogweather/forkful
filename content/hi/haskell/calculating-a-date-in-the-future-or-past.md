---
title:                "भविष्य या भूतकाल में एक तारीख की गणना"
html_title:           "Haskell: भविष्य या भूतकाल में एक तारीख की गणना"
simple_title:         "भविष्य या भूतकाल में एक तारीख की गणना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## Kyun
Kya aapne kabhi socha hai ki aapke paas ek sakt date calculator hai jo aapko future ya past ki tarikh ko calculate karne mein madad kar sakta hai? Haskell programming language ke saath, aap bilkul ye kar sakte hain. Par kyun karna chahiye ye sab? Isse aap apne coding skills ko improve kar sakte hain, aur badi sahi tarah se dates ko handle kar sakte hain.

## Kaise Kare
Sabse pehle, aapko Haskell programming language ke basic concepts aur syntax ko samajhna hoga. Agar aapne abhi tak Haskell coding nahi kiya hai, to ek simple tutorial follow karke aap iske base mein strong ho sakte hain.

Ek date ko calculate karne ke liye, hum base date, jaise ki aaj ka date, aur fir usko add ya subtract karenge, jaise ki 6 din ya 1 mahina. Iske liye, ```Data.Time``` library ka use karenge.

```Haskell
import Data.Time

-- Let's define today's date
baseDate = fromGregorian 2021 8 10 

-- 6 days in the future
futureDate = addDays 6 baseDate 
print futureDate -- Output: 2021-08-16

-- 1 month in the past
pastDate = addGregorianMonthsClip (-1) baseDate
print pastDate -- Output: 2021-07-10
```

## Deep Dive
Isse zyada complex tarikh calculation jaise leap years aur different time zones ke liye, ```Data.Time.Calendar.OrdinalDate``` aur ```Data.Time.Clock``` libraries bhi use kiye jaa sakte hain. Aap in libraries ke documentation aur code examples par dhyan dekar apne skills ko aur badha sakte hain.

Iss tarah se, aap Haskell programming language ke sath future ya past dates ko calculate kar sakte hain, aur ye aapke coding expertise ko bhi improve karega.

## Dekhiye Bhi
- [Learn You a Haskell](http://learnyouahaskell.com/)
- [Haskell Wiki](https://wiki.haskell.org/)