---
title:                "לבדיקה האם תיקייה קיימת"
html_title:           "Fish Shell: לבדיקה האם תיקייה קיימת"
simple_title:         "לבדיקה האם תיקייה קיימת"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## למה

אז למה בכלל צריך לבדוק אם תיקייה קיימת? יתכן שצריך להבין אם ומתי תיקייה קיימת על מנת להגביל את הפעולות שנעשות עליה או כדי לבדוק אם כבר טיפול מתאים נעשה על ידנו. לדוגמה, as it can help limit the actions performed on it or check if a suitable action has already been taken on it. For example, if we are trying to create a new directory, we may want to first check if it already exists to avoid creating duplicates.

## איך לבדוק באמצעות פיק ירס

אם אתם משתמשים בשפת תכנות פיש של, אתם יכולים לבדוק עם תיקייה קיימת באמצעות הפקודה `test`. כדי לבדוק אם תיקייה קיימת באמצעות פיק, ניתן להשתמש בפרמטר `-d` ואחריו לציין את שם התיקייה שאנחנו רוצים לבדוק. לדוגמה:

```
Fish Shell
```
test -d myfolder
```

אם התיקייה `myfolder` קיימת, נקבל כתוצאה את הערך 0, מה שאומר שהבדיקה הצליחה. אם התיקייה לא קיימת, נקבל כתוצאה את הערך 1, מה שאומר שהבדיקה נכשלה. כמו כן, ניתן לשלב את פקודת הבדיקה עם פקודות אחרות על מנת לבצע פעולות מתאימות בהתאם לתוצאת הבדיקה.

## עומק ועוד מידע

כדי לאתר את העזרה האפשרית עבור הדרך בה פיק משתמשת בגאנו, ניתן לבקר בדף הפרוייקט שלה על GitHub: [פרויקט פיק בגאנו](https://github.com/fish-shell/fish-shell/wiki/Glossary#-gol). כאן ניתן למצוא מידע מפורט על השימוש בפקודת הבדיקה `test` ועוד מדריכים נוספים על פיק ופקודות נוספות.

## ראו גם

- דף פיק בגאנו: https://github.com/fish-shell/fish-shell/wiki/Glossary#-gol
- פיק פרויקט בגאנו: https://github.com/fish-shell/fish-shell