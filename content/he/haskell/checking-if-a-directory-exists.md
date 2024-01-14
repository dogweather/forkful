---
title:    "Haskell: בדיקת קיום תיקייה"
keywords: ["Haskell"]
---

{{< edit_this_page >}}

### מדוע: למה לבדוק אם תיקייה קיימת

כשאנו פותחים יישומים או כתובות קוד ב-Haskell, אנו צריכים לוודא שהתיקיות שאנו משתמשים בהם קיימים כדי למנוע מטעויות ליבוד. בדיקת קיומו של תיקייה היא צעד חשוב בבנית יישומים יציבים וקוד מסודר.

### איך לבדוק אם תיקייה קיימת

המודול System.Directory מכיל פונקציה פשוטה לבדיקת קיום תיקייה בשם doesDirectoryExist. לפני שנשתמש בפונקציה זו, נצטרך לייבא את המודול על ידי הוספת השורה הבאה לראש הקובץ: 

```Haskell
import System.Directory
``` 

ניצור כעת פונקציה שתבצע את הבדיקה ותחזיר תוצאת בוליאנית (אמת או שקר) בהתאם לקיום התיקייה:

```Haskell
checkDirectory :: String -> IO Bool
checkDirectory path = do
  exists <- doesDirectoryExist path
  return exists
```

לאחר מכן, נוכל לקרוא לפונקציה ולהעביר אליה את הנתיב של התיקייה שברצוננו לבדוק:

```Haskell
main :: IO ()
main = do
  let path = "/Users/username/Documents"
  exists <- checkDirectory path
  if exists
    then putStrLn "התיקייה קיימת!"
    else putStrLn "התיקייה לא קיימת!"
```

פלט:

```
התיקייה קיימת!
```

### להעמקה

כעת נשתקף את הקוד לעומק יותר. פונקצית doesDirectoryExist מחזירה את התוצאה של פקודת המערכת stat על הנתיב שניתן כפרמטר. התוצאה היא מסדרה של סימני מחרוזת שתיארים את התכולה של התיקייה, כולל הגדרות להרשאות התיקייה ותאריך השינוי האחרון. אם התוצאה מתאימה לתיקייה שקיימת, הפונקציה תחזיר True, אחרת תחזיר False.

### ראו גם

- [מדריך למודול System.Directory](https://hackage.haskell.org/package/directory/docs/System-Directory.html)
- [מדריך לפקודת המ