---
title:                "בדיקת קיום תיקייה"
html_title:           "Gleam: בדיקת קיום תיקייה"
simple_title:         "בדיקת קיום תיקייה"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## מה ולמה?

"בדיקת קיום תיקייה" היא פעולה שמאפשרת למתכנתים לבדוק אם תיקייה מסוימת קיימת במערכת הקבצים. זה יכול להיות שימושי במקרים של בדיקת תנאים לפני הרצת קוד או בודקי אבטחה שנרחבו לבדוק אם קבצים חשובים קיימים.

## איך לבדוק קיום תיקייה ב-Gleam?

```Gleam
directory_check(path) ->
    if file.exists(path) do
        Ok("The directory exists.")
    else
        Err("The directory does not exist.")
    end

directory_check("my_directory")
```

פלט:

```Gleam
Ok("The directory exists.")
```

## צלילה לעומקים

(1) בדיקת קיום תיקייה היא פעולה יישומית שנמצאת בשימוש במגוון תחומים, כולל מערכות הפעלה ואבטחת מידע. (2) חלופות לבדיקת קיום תיקייה כוללות אימות יצירת התיקייה עצמה או ניהול של תנאי מגוזרים. (3) ברקיע, פעולת בדיקת קיום תיקייה מבוצעת על ידי בדיקת הפרוטוקול הבסיסי COMMAND_EXISTING במערכות הפעלה של ג'ל.

## ראו גם

למידע נוסף על פעולת בדיקת קיום תיקייה ב-Gleam, יש לעיין בקוד המקור המלא והתיעוד הרשמי בגיטהאב של הפרויקט.

[קוד המקור ב-GitHub] (https://github.com/gleam-lang/gleam/blob/master/src/gleam/file.erl)

[תיעוד רשמי] (https://gleam.run/documentation/#file.exists)