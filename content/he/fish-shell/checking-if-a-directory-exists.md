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

## מה ולמה?

בדיקה אם תיקייה קיימת היא תהליך פשוט שבודק אם תיקייה קיימת במערכת הקבצים. תהליך זה חשוב למתכנתים כי הוא מאפשר להתחשב במצבים שונים ולטפל בהם באופן תקין.

## איך לעשות?

כדי לבדוק אם תיקייה מסוימת קיימת ב-Fish Shell, יש להשתמש בפקודת "test -d" ולשים בתוכה את שם התיקייה שברצוננו לבדוק. לדוגמה:

```
test -d Documents
```

הפקודה תחזיר true אם התיקייה קיימת ו-false אם אינה קיימת. ניתן גם להשתמש באופרטור "&&" כדי לבדוק אם התיקייה קיימת ולבצע פעולה מסוימת במקרה שהיא קיימת, לדוגמה:

```
test -d Downloads && echo "נמצא תיקייה Downloads במערכת הקבצים שלך"
```

## העמקה נוספת

בדיקה אם תיקייה קיימת היא חלק חשוב מהשגרה היומיומית של מתכנתים. בעזרת פקודת test הם יכולים לטפל במצבים שונים ולבצע פעולות בהתאם לתוצאה של הבדיקה.

בנוסף לשימוש ב test -d, ישנן פקודות נוספות לבדיקת תיקיות כמו test -e (בדיקה אם קובץ או תיקייה קיימים) ו-test -f (בדיקה אם קובץ רגיל קיים). ישנן גם אפשרויות לבדיקת הרשאות גישה לתיקיות וקבצים.

## ראה גם

למידע נוסף על השימוש בפקודת test ב-Fish Shell, ניתן לקרוא את המדריכים הבאים:

- [מסמך רשמי של Fish Shell על פקודת test](https://fishshell.com/docs/current/cmds/test.html)
- [מדריך קצר על שימוש ב-IF ב-Fish Shell](https://medium.com/@gamunu/quick-tip-if-conditional-in-fish-shell-and-boolean-operators-5af9b7a64410)
- [דוגמאות נוספות לשימוש בפקודת test ב-Fish Shell](https://stackoverflow.com/questions/31000507/how-to-check-if-a-file-exists-in-fish-shell)