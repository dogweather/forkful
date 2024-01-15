---
title:                "יצירת קובץ זמני"
html_title:           "Fish Shell: יצירת קובץ זמני"
simple_title:         "יצירת קובץ זמני"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

# למה

במקרים מסוימים, יצירת קובץ זמני יכולה להיות שימושית לצורך גישה לפרטי מידע מזמן לזמן בתוך סקריפטים או תוכניות שונות.

## איך לעשות זאת

קוד לדוגמה: ```fish shell echo "מקור המידע" > temp.txt```

פלט: ```temp.txt```

קוד לדוגמה: ```fish shell cat temp.txt```

פלט: ```מקור המידע```

קוד לדוגמה: ```fish shell rm temp.txt```

פלט: הקובץ הזמני יימחק.

## ירידה מסוקרת

יצירת קובץ זמני באמצעות פיש של כמה שניות מאפשר למשתמש לגשת למידע בצורה נוחה לפי צורך, בלי להשתמש בקבצים קבועים שעשויים לטעות ולהוציא מילון מהמטרות המדויקות שלו. פיש של מבני נתונים גמישים והיצירה של קובץ זמני היא דוגמה נהדרת להשתמש בתכונות הסקריפט שלו.

# ראו גם

- פנקציית tempfile של פיש - https://fishshell.com/docs/current/cmds/tempfile.html
- מדריך מתקדם לפעולות בשפת פיש - https://fishshell.com/docs/current/index.html