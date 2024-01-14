---
title:                "Haskell: המרת תאריך למחרוזת"
programming_language: "Haskell"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

# למה:

אחת התכונות הנפוצות בשפת פונקציונלית כמו Haskell היא היכולת להמיר משתנים מסוג אחד למשתנים מסוג אחר. בפוסט הזה נתמקד בהמרת תאריכים למחרוזות ונראה איך ניתן לעשות זאת באמצעות Haskell.

# כיצד להמיר תאריך למחרוזת:

מתחילים עם השתמשות בפונקציה `show`. הפונקציה הזאת מקבלת אובייקט מסוג "תאריך" ומחזירה מחרוזת המכילה את התאריך בפורמט של dd/mm/yyyy. לדוגמה, ננסה להמיר את התאריך  22/08/2021:

```Haskell
show (22, 8, 2021)
```
הפלט שנקבל הוא: `"22/08/2021"`

במקום לפנות ישירות לפונקציה `show`, נוכל לכתוב פונקציה עצמאית המקבלת את הפורמט של התאריך שנרצה להמיר ומעבירה אותו לפונקציה `show`. לדוגמה, נבקש מהמשתמש להזין את התאריך ונשתמש בפונקציה `getLine` כדי לקבל את הקלט מהמשתמש:

```Haskell
import System.IO

main = do
    putStrLn "נא להזין תאריך (dd/mm/yyyy): "
    dateText <- getLine
    let date = read dateText :: (Int, Int, Int) -- המרת הטקסט למשתנה של תאריך
    let format = "%d/%m/%Y"
    let dateString = formatCalendarTime defaultTimeLocale format date
    putStrLn dateString 
```

כאן אנחנו משתמשים בפונקציה `read` כדי להמיר את הטקסט שהמשתמש הזין למשתנה של תאריך. נעביר את המשתנה הזה לפונקציה `formatCalendarTime` עם הפורמט של התאריך שנרצה לקבל. במקרה הזה, הפורמט הוא "%d/%m/%Y" שמתאים לתאריך בפורמט של dd/mm/yyyy. בסוף, הפלט שנקבל הוא התאריך כמחרוזת בפורמט שבחרנו.

# מעמקים:

קיימת כמה שיטות נוספות להמרת תאריך למ