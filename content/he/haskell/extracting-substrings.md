---
title:    "Haskell: חילוץ תת-מחרוזות"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

## למה

החיתוך של מחרוזות הוא כלי חשוב בכתיבת קוד ביינים. הוא מאפשר לנו לקבל תת מחרוזת מתוך מחרוזת גדולה יותר, מה שמאפשר לנו לעבוד על חלקים מסוימים במחרוזת באופן נפרד. כתיבת פונקציה שמחלצת מחרוזת תת מחרוזת, תוך שמירה על קוד נקי ואיכותי, היא חשובה ומומלצת לכל מתכנת.

## כיצד לעשות זאת

הנה דוגמא קצרה של כיצד לכתוב פונקציה ב-Haskell שמחלצת תת מחרוזת מתוך מחרוזת:

```Haskell
-- פונקציה שמחלצת תת מחרוזת מתוך מחרוזת
extractSubstring :: String -> String -> String
extractSubstring s s' = drop (length s) (take (length s') s')
```

### דוגמאות להרצה:

```Haskell
extractSubstring "Haskell" "Hello, Haskell!" -- פלט יהיה "Hello, "
extractSubstring "ים" "בקיץ נהנים בים" -- פלט יהיה "נהנים ב"
```

וכאן דוגמא מיוחדת למתכנתים בהריידים – איך להשתמש בפונקציה `drop` ובפונקציה `take` כדי לחלץ תא מסוים מהרשימה המכילה את האתרים השונים:

```Haskell
websites = ["https://www.google.com/", "https://www.facebook.com/", "https://www.youtube.com/"]

-- פונקציה שמחלצת את האתרים מהרשימה הנתונה שמתחילים ב-https
extractWebsites :: [String] -> [String]
extractWebsites xs = map (drop 8) (filter (take 8) xs)
```

### פלט יהיה:

```Haskell
extractWebsites websites -- פלט יהיה ["www.google.com/", "www.facebook.com/", "www.youtube.com/"]
```

## לחקור עוד

לחיתוך מחרוזות יש מבנה מורכב יותר ממה שראינו עד כה. הפונקציות `drop` ו-`take` הן רק חלק קטן מכלי החיתוך שלנו. ניתן גם להשתמש בפונקציות נוספות כמו `substring` ו-`split` על מנת לקבל תוצאות עוד יותר מדויקות. כדאי לחקור מהוויקי הרש