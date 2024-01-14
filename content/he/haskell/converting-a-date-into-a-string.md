---
title:    "Haskell: המרת תאריך למחרוזת"
keywords: ["Haskell"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/haskell/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## למה

למה לבצע המרה של תאריך למחרוזת? ב-Haskell, לנו יש צורך לעבוד עם תאריכים שונים באופן קבוע, לדוגמה בתוך מערכות פיננסיות או ליצירת דוחות. כמו כן, בעבודה עם API אנו נדרשים לשלוח תאריכים בתבנית מסוימת כדי לקבל תשובות תקינות. לכן, ייתכן ונחוץ להמיר תאריך למחרוזת ולהתאים אותו לצרכים המיוחדים שלנו.

## איך לבצע

נדגים את התהליך של המרת תאריך למחרוזת באמצעות הפונקציה `formatTime` הכתובה בנוסחת Haskell. נשתמש בתאריך נוכחי כדי להדגים את הפעולה ונציג את התוצאות במבנה תאריך פשוט.

```Haskell
import Data.Time.Format  
import Data.Time.LocalTime 

-- קביעת תאריך נוכחי 
currentDate :: IO String  
currentDate = do  
    -- השמת תאריך נוכחי בטווח זמן ישראל 
    currentTime <- getCurrentTime  
    let time = DubaiTimeZone 
    -- המרה למחרוזת מבחינה פורמטית 
    return $ formatTime defaultTimeLocale "%-d/%-m/%Y" $ utcToLocalTime time currentTime 

-- הדפסת התאריך הנוכחי בפורמט מחרוזת פשוט    
main :: IO ()  
main = do  
    dateStr <- currentDate  
    putStrLn $ "התאריך הנוכחי הוא: " ++ dateStr  
```

תוצאה:

`התאריך הנוכחי הוא: 7/12/2021`

## נחילות

הפונקציה `formatTime` מופעלת על ידי פורמט תורף שמכיל מחרוזת של פקודות כמו יום (%-d), חודש (%-m) ושנה (%Y) ומשתמשת בטווח זמנים כדי להמיר את התאריך לתוצאה המוגדרת. ניתן גם לבצע המרה לפורמטים שונים כגון צורה אמריקאית (MM-dd-yyyy) או אירופאי (dd/MM/yyyy) על ידי שימוש בפקודות הנכונות. כמו כן, ניתן להשתמש בפונקציות נוספות לעיבוד תאריכ