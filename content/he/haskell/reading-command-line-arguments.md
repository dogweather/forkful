---
title:                "קריאה של ארגומנטים משורת הפקודה"
html_title:           "C#: קריאה של ארגומנטים משורת הפקודה"
simple_title:         "קריאה של ארגומנטים משורת הפקודה"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## מה ולמה? 
קריאת ארגומנטים משורת הפקודה היא התהליך שבו התוכנית מקבלת נתונים בעת הרצה. מתכנתים עושים זאת כדי לקבוע את התנהגות התוכנית, מבלי לשנות את הקוד בכל פעם.

## איך?
כאן הולך להיות חלק של דוגמאות קוד המראות איך להשתמש בדפוס 'getArgs' בהשקל.

```Haskell
import System.Environment
main :: IO ()
main = do
    args <- getArgs
    print args
```
אם נריץ את התוכנית שקלה עם "hello" ו "world" כארגומנטים, הפלט יהיה:
```bash
["hello", "world"]
```

## צלילה עמוקה
עם ההופעה הראשונה של ביאן פלטר ב-1990, השפה Haskell שימשה כמרכז לרעיונות חדשים בתחום התכנות הפונקציונלי. קריאת ארגומנטים משורת הפקודה היא אחת התכונות שהובאו משפות תכנות אחרות.

מעבר ל- 'getArgs', ישנם שיטות נוספות לקרוא את הארגומנטים, כמו 'getProgName' ו 'getEnv'. המשתמש יכול גם להשתמש בהכנות אומנמות לקבוע את הארגומנטים.

פרטים על המימוש: 'getArgs' היא פונקציה של קלט/פלט, כך שהיא חייבת להיכנס תחת ה- 'do'. 

## קרא גם
בהמשך, ממליצים לקרוא את המקורות הבאים:
* מדריך כתיבת היישומים המקובלת (Real World Haskell): http://book.realworldhaskell.org/
* קורס Haskell שווה אשר מתמקד בבנייה של אפליקציות לפרודוקשן: https://www.seas.upenn.edu/~cis194/fall16/
* פרטים נוספים על 'System.Environment': https://hackage.haskell.org/package/base-4.2.0.1/docs/System-Environment.html