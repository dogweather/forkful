---
date: 2024-01-26 04:32:27.745403-07:00
description: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD XML \u05D1\u05D4\u05D0\u05E1\
  \u05E7\u05DC \u05DB\u05D5\u05DC\u05DC\u05EA \u05E0\u05D9\u05EA\u05D5\u05D7, \u05E2\
  \u05D9\u05D1\u05D5\u05D3 \u05D5\u05D9\u05E6\u05D9\u05E8\u05EA \u05DE\u05D1\u05E0\
  \u05D9 XML. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05EA\u05DE\u05D5\u05D3\
  \u05D3\u05D9\u05DD \u05E2\u05DD XML \u05DB\u05D3\u05D9 \u05DC\u05D4\u05EA\u05E7\u05E9\
  \u05E8 \u05E2\u05DD \u05DE\u05D2\u05D5\u05D5\u05DF \u05D9\u05D9\u05E9\u05D5\u05DE\
  \u05D9\u05DD \u05D5\u05E4\u05E8\u05D5\u05D8\u05D5\u05E7\u05D5\u05DC\u05D9\u05DD\
  \ \u05E9\u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1-XML \u05DB\u05E4\u05D5\
  \u05E8\u05DE\u05D8 \u05D4\u05E0\u05EA\u05D5\u05E0\u05D9\u05DD\u2026"
lastmod: '2024-03-13T22:44:39.452346-06:00'
model: gpt-4-0125-preview
summary: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD XML \u05D1\u05D4\u05D0\u05E1\
  \u05E7\u05DC \u05DB\u05D5\u05DC\u05DC\u05EA \u05E0\u05D9\u05EA\u05D5\u05D7, \u05E2\
  \u05D9\u05D1\u05D5\u05D3 \u05D5\u05D9\u05E6\u05D9\u05E8\u05EA \u05DE\u05D1\u05E0\
  \u05D9 XML."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD XML"
weight: 40
---

## מה ולמה?

עבודה עם XML בהאסקל כוללת ניתוח, עיבוד ויצירת מבני XML. מתכנתים מתמודדים עם XML כדי להתקשר עם מגוון יישומים ופרוטוקולים שמשתמשים ב-XML כפורמט הנתונים שלהם, כמו שירותי רשת וקבצי תצורה.

## איך לעשות:

האסקל מציע ספריות כמו `xml-conduit` לעבודה עם XML. הדוגמה הבאה מדגימה ניתוח מחרוזת XML ושאילתת אלמנטים:

```haskell
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as T
import Text.XML
import Text.XML.Cursor

main :: IO ()
main = do
  let xmlContent = "<greetings><hello>World!</hello></greetings>"
  let document = parseLBS_ def $ T.encodeUtf8 $ T.pack xmlContent
  let cursor = fromDocument document

  let helloTexts = cursor $// element "hello" &/ content
  print helloTexts  -- ['World!']
```

פלט לדוגמה:

```
["World!"]
```

## צלילה עמוקה

XML, שמכונה בשמו המלא שפת סימון מורחבת, היה בסיסי בסריאליזציה של נתונים הרבה לפני עלייתו של JSON. הוא מפורט, אך קפדני ומתוקנן, מה שהופך אותו למתאים לסביבות עסקיות נוקשות, מערכות ישנות ותעשיות כמו פיננסים ובריאות.

להאסקל יש מספר ספריות ל-XML; עם זאת, `xml-conduit` היא בין החזקות והנפוצות ביותר בזכות יכולות הניתוח והשידור היעילות שלה, חלק ממשפחת `conduit` לטיפול בזרמי נתונים.

חלופות כוללות את `HXT` (כלי עבודה ל-XML בהאסקל) אשר משתמש בחצים לניתוח והמרה, ומציע פרדיגמה שונה למניפולציות ב-XML. למרות ש-`HXT` פחות פופולרית כיום בגלל עקומת הלמידה התלולה יותר, עדיין מדובר בבחירה קבועה לחלק מהמקרים.

כאשר מיישמים עיבוד XML בהאסקל, יש להתייחס לקידוד, מכיוון שמחרוזות האסקל הן ביוניקוד ונתוני ה-XML עשויים שלא להיות כאלה. כמו כן, מרחבי שמות XML יכולים להוסיף מורכבות נוספת לניתוח.

## ראו גם:

- תיעוד החבילה `xml-conduit`: https://hackage.haskell.org/package/xml-conduit
- כלי העבודה ל-XML של האסקל (HXT): http://hackage.haskell.org/package/hxt
- הספר "האסקל בעולם האמיתי", פרק 16, לעיבוד XML: http://book.realworldhaskell.org/read/xml.html
- הוויקי של האסקל על XML: https://wiki.haskell.org/XML
