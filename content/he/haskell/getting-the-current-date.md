---
title:                "קבלת התאריך הנוכחי"
html_title:           "C#: קבלת התאריך הנוכחי"
simple_title:         "קבלת התאריך הנוכחי"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## מה ולמה?

משימה פשוטה שהמתכנתים מבצעים שוב ושוב היא קבלת התאריך הנוכחי. זה עוזר לנו לדעת מתי הוצעו נתונים, לסמן מועדים, או למדוד כמה זמן עבר.

## איך מבצעים זאת:

אז איך אנו מקבלים את התאריך הנוכחי ב־Haskell? בואו נראה דוגמה עם הספרייה `Data.Time`.

```Haskell
import Data.Time

main :: IO ()
main = do
   current <- getCurrentTime
   print current
```

פלט לדוגמה:

```Haskell
2022-05-09 12:46:00 UTC
```

## חקירה מעמיקה:

הצורה המקובלת לקבלת התאריך הנוכחי התפתחה מהצורך לקבלת מידע מתוארזם.

החלופות לקבלת הזמן הנוכחי הן לבנות מנגנון מותאם אישית, או להשתמש בספריות של שפות אחרות.

תחת המסך, `getCurrentTime` מושך את הזמן מהמערכת המרחיבה וממשטרת זאת בעזרת הספרייה 'Data.Time'.

## ראה גם:

ניתן למצוא מקורות נוספים בנושא באינטרנט, כולל תיעוד, מדריכים ומאמרים המסבירים בהרחבה:

- [Bedrock-time: ראשית מאמרים של Haskell(hackage.haskell.org)](http://hackage.haskell.org/package/time)
- [טיפים לתכנות Haskell למתכנתים מתחילים (www.haskell.org)](https://www.haskell.org/haskellwiki/Haskell_in_5_steps)
- [כתיבת ביטויים בהכנת תאריך ב־ Haskell (stackoverflow.com)](https://stackoverflow.com/questions/30229412/writing-date-expressions-in-haskell)