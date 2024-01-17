---
title:                "एक तारीख को स्ट्रिंग में रूपांतरण करना"
html_title:           "Haskell: एक तारीख को स्ट्रिंग में रूपांतरण करना"
simple_title:         "एक तारीख को स्ट्रिंग में रूपांतरण करना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

Kya aur Kyon?

Date ko string mein convert karna ek aam tarika hai jo programmers apne code mein use karte hain. Isse hum date ko alag-alag formats mein display kar sakte hain jaise ki 'dd/mm/yyyy' ya 'mm/dd/yyyy'. Isse code ki readability bhi badhti hai aur code maintenance bhi aasan ho jaata hai.

Kaise karein?

```Haskell
import Data.Time
import Data.Time.Format

-- Convert current date into string
dateAsString :: IO String
dateAsString = do
  now <- getCurrentTime
  return $ formatTime defaultTimeLocale "%d/%m/%Y" now
```

Sample output: 02/05/2021

```Haskell
import Data.Time
import Data.Time.Format

-- Convert a given date into string
dateAsString :: Day -> String
dateAsString day = formatTime defaultTimeLocale "%A, %d %B %Y" day
```

Sample output: Sunday, 02 May 2021

Maze mein dubein!

Is technique ko use karne se pehle, hume Data.Time aur Data.Time.Format modules ko import karna hoga. Iske baad hume `formatTime` function ka use karke desired format mein date ko convert karna hoga.

Iske alawa, hum khud se bhi ek custom format create kar sakte hain jaise ki 'dth month yyyy', jo ki output mein "2nd May 2021" dega. Iske liye, hum `%e` ko `%d` ke jagah use karenge aur `%B` ko `%B` ke jagah `%{th,nd,rd}` add karenge.

See Also:

- Date, DateTime and Timestamp in Haskell: https://www.geeksforgeeks.org/date-datetime-and-timestamp-in-haskell/
- The official documentation for Data.Time module: https://hackage.haskell.org/package/time-1.9.3/docs/Data-Time.html