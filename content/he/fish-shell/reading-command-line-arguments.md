---
title:                "קריאה של ארגומנטים משורת הפקודה"
html_title:           "C#: קריאה של ארגומנטים משורת הפקודה"
simple_title:         "קריאה של ארגומנטים משורת הפקודה"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## מה ולמה?
קריאת פרמטרים מהשורה הפקודה היא האופן שבו תוכנה מקבלת נתונים מהמשתמש. זה מאפשר למתכנתים למנות על הדרך בה התוכנה שלהם תשתמש באינפוט.

## איך נעשה:
הנה מספר דוגמאות לקוד שמראה איך לקרוא פרמטרים מהשורה הפקודה ב-Fish Shell:

```Fish Shell
# הדפסת פרמטרים אחד אחרי השני
for param in $argv
    echo $param
end
```

אם נריץ את האסך הזה עם הפרמטרים "אחד", "שני", "שלוש", ההדפסה תהיה:
```
אחד
שני
שלוש
```

## צלילה עמוקה:
השפה Fish Shell מגיעה להשלמה אחרי שנים של התפתחות של שפות סקריפט ל-shell. השפה פותחה בתכלית להיות יעילה ולהכיל תכונות שימושיות.

כאופציה אחרת, אפשר להשתמש גם ב- Bash או Zsh לקריאת פרמטרים מהשורה הפקודה. למשל, ב-Bash אפשר להשתמש ב- "$@" או ב- "$1", "$2", וכו'.

ב-Fish Shell, אם נעבור יותר מ-100 פרמטרים, המערכת יכולה להיות לא מדויקת בגלל ההבדלים במהלך קריאת המערכת.

## ראה גם:
* [תיעוד Fish Shell](https://fishshell.com/docs/current/index.html)
* [Fish Shell בגיטהאב](https://github.com/fish-shell/fish-shell)
* [שפת סקריפט של Bash](https://www.gnu.org/software/bash/)
* [Zsh](http://zsh.sourceforge.net/Doc/Release/zsh_toc.html)