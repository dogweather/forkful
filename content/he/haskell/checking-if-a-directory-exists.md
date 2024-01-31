---
title:                "בדיקה האם תיקייה קיימת"
date:                  2024-01-20T14:56:44.645576-07:00
html_title:           "Gleam: בדיקה האם תיקייה קיימת"
simple_title:         "בדיקה האם תיקייה קיימת"

category:             "Haskell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/haskell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## מה ולמה?
בדיקת קיום תיקייה היא אופן שבו תוכנית יכולה לוודא אם תיקייה נמצאת או לא במערכת הקבצים. תכניתאים עושים זאת למנוע שגיאות, כמו ניסיון כתיבה לתיקייה שלא קיימת.

## איך לעשות:
ב-Haskell, נבדוק את קיום התיקייה עם הפונקציה `doesDirectoryExist` מהמודול `System.Directory`.

```haskell
import System.Directory (doesDirectoryExist)

main :: IO ()
main = do
    let dirPath = "/path/to/directory"
    exists <- doesDirectoryExist dirPath
    putStrLn $ "Does the directory exist? " ++ show exists
```

פלט דוגמא אם התיקייה קיימת:

```
Does the directory exist? True
```

פלט דוגמא אם התיקייה לא קיימת:

```
Does the directory exist? False
```

## עיון מעמיק
`doesDirectoryExist` היא חלק מהספריה הסטנדרטית של Haskell ומספקת דרך נוחה לבדוק קיום של תיקיות. בהיסטוריה, כללו שפות תכנות שונות דרכים שונות לשאול את מערכת הקבצים, אבל התקדמות טכנולוגית אפשרה גישה יותר פשוטה ואינטואיטיבית.

חלופות ל-`doesDirectoryExist` כוללות יצירת תיקייה עם `createDirectoryIfMissing` שמבטיחה שתיקייה תהיה קיימת או שימוש ב-funcation `catch` לניהול מצבים בהם התיקייה לא קיימת.

הפונקציה `doesDirectoryExist` עובדת על ידי שליחת קריאת מערכת על מנת לקבל את פרטי המדד של התיקייה. זה ידרוש גישה למערכת הקבצים ולכן יכול להיות לאט בהשוואה לזיכרון או פעולות מבוססות מחשב.

## ראה גם
* המסמכים הרשמיים של [`System.Directory`](https://hackage.haskell.org/package/directory-1.3.6.1/docs/System-Directory.html) מחבילת Hackage.
* מדריך בעברית להתחלה עם Haskell: [Learn You a Haskell for Great Good! (בעברית)](http://learnyouahaskell.com/chapters).
* קבוצת המשתמשים של Haskell בישראל: [Israel Haskell User Group](https://www.meetup.com/Israel-Haskell-Users-Group/).
