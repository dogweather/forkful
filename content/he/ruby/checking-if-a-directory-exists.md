---
title:                "בדיקת קיום תיקייה"
html_title:           "Ruby: בדיקת קיום תיקייה"
simple_title:         "בדיקת קיום תיקייה"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/ruby/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## למה

אנשים יכולים לצרוך לבדוק אם תיקייה קיימת על מנת לוודא שהקובץ שהם מנסים לקרוא או לכתוב אליו קיים במחשבם. זה יכול להיות חשוב במיוחד באפליקציות שמתאפיינות בקריאה וכתיבה של קבצים.

## כיצד לעשות זאת

כדי לבדוק אם תיקייה קיימת באמצעות פייתון, ניתן להשתמש בפונקציית `Dir.exist?()` ולמסור לה את נתיב התיקייה שאנחנו רוצים לבדוק. פונקציית זו תחזיר את הערך `true` אם התיקייה קיימת ואת הערך `false` אם היא לא קיימת. לדוגמה:

``` Ruby
if Dir.exist?('/Users/username/Documents')
    puts "The directory exists!"
else
    puts "Sorry, the directory does not exist."
end
```

פקודה זו תדפיס "The directory exists!" אם התיקייה קיימת ואת "Sorry, the directory does not exist." אם היא לא קיימת.

## כידורס עמוק

בנוסף לפונקציית `Dir.exist?()` ישנם גם אופציות נוספות לבדיקת תיקייה קיימת בפייתון. למשל, ניתן להשתמש בפונקציית `File.exist?()` על מנת לבדוק אם קובץ קיים במקום שנמצא בתוך התיקייה המבוקשת. עוד אפשרות היא להשתמש בפונקציית `Dir.entries()` כדי לקבל רשימת כל הקבצים והתת-תיקיות בתיקייה נתונה.

## ראו גם

- תיעוד רשמי של פייתון עבור פונקציית `Dir.exist?()`: https://ruby-doc.org/core-3.0.2/Dir.html#method-c-exist-3F
- מדריכים נוספים לפייתון: https://www.rubyguides.com/