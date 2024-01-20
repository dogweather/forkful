---
title:                "בדיקה האם תיקייה קיימת"
date:                  2024-01-20T14:56:35.865395-07:00
html_title:           "Gleam: בדיקה האם תיקייה קיימת"
simple_title:         "בדיקה האם תיקייה קיימת"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why?
מה זה בדיקת קיום תיקייה ולמה זה חשוב? בדיקת קיום תיקייה ב-Shell מאפשרת לנו לוודא שהתיקייה אותה אנו רוצים להשתמש בה אכן קיימת על המערכת. זה חיוני למניעת שגיאות בתהליך הרצה של סקריפטים ותכניות.

## How to:
ב-Fish Shell, תוכלו לבדוק אם תיקייה קיימת בצורה הבאה:

```Fish Shell
if test -d /path/to/directory
    echo "Directory exists"
else
    echo "Directory does not exist"
end
```

פלט דוגמה אם התיקייה קיימת:
```
Directory exists
```

פלט דוגמה אם התיקייה אינה קיימת:
```
Directory does not exist
```

## Deep Dive
בדיקה של קיום תיקייה היא דבר שמתכנתים עושים כבר מאז הימים הראשונים של מערכות הפעלה עם ממשק שורת פקודה. ב-Fish Shell, הפקודה `test` מספקת אמצעי לבדוק על תנאים שונים, כאשר האופציה `-d` בודקת ספציפית עבור תיקיות. אלטרנטיבות כוללות שימוש בפקודה `[` או `[[` בשלים אחרים כמו Bash, אבל Fish לא תומך בתחביר זה מבלי לציין את הפקודה `test`. פרטי המימוש של הבדיקה הזו נובעים מהקריאה לפונקציונליות המערכת המבוססת על Syscalls שמאפשרת לקבל מידע על קבצים ותיקיות.

## See Also
- דוקומנטציית ה-Fish Shell על הפקודה `test`: [Fish Shell Documentation on test](https://fishshell.com/docs/current/commands.html#test)
- מדריך לבדיקת תנאים ב-Fish Shell: [fishshell.com - Tutorial: Conditionals](https://fishshell.com/docs/current/tutorial.html#tut_conditionals)