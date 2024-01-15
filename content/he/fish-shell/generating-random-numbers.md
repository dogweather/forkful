---
title:                "יצירת מספרים אקראיים"
html_title:           "Fish Shell: יצירת מספרים אקראיים"
simple_title:         "יצירת מספרים אקראיים"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

**## למה**

מספרים אקראיים הם כלי חשוב בתכנות ובתחום המחשבים בכלל. הם משמשים ליצירת פעמונים שונים בתוך קוד, כגון תנאים אקראיים ופעולות מעורבות.

**## איך לעשות**

ניתן ליצור מספרים אקראיים באמצעות שפת תסריט ה- Fish Shell. נתחיל עם פקודת "math srand" שמגדירה את התעריף המשתנה שיוכל לייצר את המספרים האקראיים. לדוגמה:

```
Fish Shell
math srand 123
echo $random
```

פקודת "echo" תדפיס מספר אקראי מתוך התעריף שנקבע. ישנן גם אפשרויות נוספות כגון "math rand" שמגרילה מספרים אקראיים עם נקבע של תחום חדש. לדוגמה:

```
Fish Shell
math rand 50 100
echo $random
```

זה יגדיר את הפקודה לגרום לפקודת "echo" להדפיס מספרים בין 50 ל-100. כל פעם שתפעילו את הקוד, תקבלו מספרים אחרים בתחום הנתון.

**## חפירה מעמיקה לתוך יצירת מספרים אקראיים**

יצירת מספרים אקראיים נעשית על ידי שימוש באלגוריתם לבחירת מספרים באופן אקראי. מומלץ להשתמש בתעריף גבוה יותר כדי לייצר מספרים אקראיים יותר מדויקים. כמו כן, כדאי להגדיר את התעריף לפני כל פעולה של מספרים אקראיים כך שתמיד תקבלו תוצאות חדשות ואקראיות.

**## ראו גם**

אתר Fish Shell הרשמי: https://fishshell.com/

מדריכים נוספים על Fish Shell: https://fishshell.com/docs/current/index.html#tutorials

מדריך של HackerNoon על איך להתחיל עם תסריטי Fish Shell: https://hackernoon.com/a-gentle-introduction-to-fish-shell-q8133y9u