---
title:                "התחלת פרויקט חדש"
aliases:
- he/fish-shell/starting-a-new-project.md
date:                  2024-01-20T18:03:24.660915-07:00
model:                 gpt-4-1106-preview
simple_title:         "התחלת פרויקט חדש"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why? מה ולמה?
כשאתה מתחיל פרויקט חדש, אתה בעצם יוצר סביבה לפיתוח קוד. תוכניתנים עושים את זה כדי לסדר את הרעיונות שלהם ולדאוג שכל המשאבים הנחוצים יהיו במקום הנכון.

## How to: איך לעשות?
דוגמאות קוד ב-Fish Shell:

```Fish Shell
# יצירת תיקייה חדשה לפרויקט
mkdir my_project
cd my_project

# הכנת סביבת פיתוח עם Git
git init

# יצירת קובץ README ראשוני
echo "# My New Project" > README.md
```

פלט דוגמה לקוד לעיל:
```Fish Shell
> mkdir my_project
> cd my_project
> git init
Initialized empty Git repository in /path/to/my_project/.git/
> echo "# My New Project" > README.md
```

## Deep Dive עומק הידע
התחלת פרויקט חדש היא לא רק יצירת תיקיות וקבצים; זה כולל הבנה של מסגרות עבודה וכלים שיעזרו לך להתקדם. פיש של - Fish Shell - הוא גרסה מתקדמת של מעטפת פקודה שנותנת דגש על ידידותיות למשתמש וחווית פיתוח נעימה. כלי תוכנה זה הוא אלטרנטיבה ל-Bash ו-Zsh. היסטורית, Fish הוא יותר חדש, הראשונה הופיעה ב-2005, והוא ממשיך לפתח פונקציונליות מתקדמת.

## See Also גם כדאי לראות
- דוקומנטציה רשמית של Fish Shell: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- השוואה בין של Fish ל-Bash: [https://hyperpolyglot.org/unix-shells#fish](https://hyperpolyglot.org/unix-shells#fish)
