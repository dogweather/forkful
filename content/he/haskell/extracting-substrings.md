---
title:                "החלצת תת-מחרוזות"
html_title:           "Haskell: החלצת תת-מחרוזות"
simple_title:         "החלצת תת-מחרוזות"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/extracting-substrings.md"
---

{{< edit_this_page >}}

## מה ולמה?
חילוץ מחרוזות הוא פעולת עיבוד מחרוזת נפוצה בתכנות. כאשר תוכניות משתמשות במחרוזות כמקור נתונים, או כאשר נדרש פילוח מחרוזת למטרות מסוימות, חילוץ מחרוזות מאפשר לנו לקבל תת מחרוזת מתוך מחרוזת גדולה יותר. זה מאפשר לנו לנהל ולעבד את המידע שלנו בצורה יעילה יותר.

## איך לעשות זאת:
כאשר מדברים על חילוץ מחרוזות בג'אווהסקריפט, מתכנתים מתייחסים לשימוש בפונקציות "substring" או "slice" כדי לקבל תת מחרוזת מתוך מחרוזת גדולה יותר. ב-Haskell, אנו משתמשים בפונקציות "take" ו-"drop" כדי לחלק את המחרוזת לחלקים ולהחזיר את החלקים הרצויים כתוצאה.

```Haskell
-- קבלת תת מחרוזת מתחילת המחרוזת
take 4 "היי, שלום!"
-- פלט: "היי"

-- קבלת חלק מהמחרוזת לאחר האינדקס הנתון
drop 2 "היי, שלום!"
-- פלט: "לום!"
```

## חידוד פרטים:
בעבר, כאשר התכנות נעשה הרבה בשפות תכנות מונחות עצמים כמו C ו-Pascal, חילוץ מחרוזות היה מסורתית נעשית באמצעות פונקציות בנויות של השפה. אבל ב-Javascript וב-Haskell, יש פונקציות ישירות עבור כך. יתר על כן, עם התפתחות שפות תכנות עם תמיכה טובה יותר בתכנות פונקציונלי, כמו Scala ו-F#, מתכנתים יכולים גם להשתמש בפונקציות טובות יותר כדי לשבש את המחרוזות.

## ראה גם:
* [מדריך לפעולות מובנות ב-Haskell](https://hackage.haskell.org/package/base-4.16.0.0/docs/Prelude.html#g:17)
* [כתב עת על חילוץ מחרוזות בג'אווהסקריפט](https://www.geeksforgeeks.org/javascript-string-prototype-slice/)
* [התרחישים השונים לחילוץ מחרוזות ב-Python, JavaScript ו-Ruby](https://www.geeksforgeeks.org/different-choices-for-extracting-substring-in-python-java-javascript-and-ruby/)