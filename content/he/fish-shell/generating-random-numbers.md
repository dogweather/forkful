---
title:                "Fish Shell: יצירת מספרים אקראיים"
programming_language: "Fish Shell"
category:             "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## מדוע

כדי להשתמש במספרים אקראיים מומלץ לפעול בשפת תכנות כגון פיש של. מספרים אקראיים יכולים לשמש למגוון רחב של מטרות, כגון אימוני משחקים או יצירת סיסמאות אקראיות לצורכי אבטחה.

## כיצד לעשות זאת

בפיש של ישנן מספר דרכים ליצור מספרים אקראיים. הנה כמה דוגמאות קוד ופלט ליצירת מספרים אקראיים:

```fish
# יצירת מספר אקראי בין 1 ל-10
set random_number (random 1 10)
echo $random_number

# יצירת מספר אקראי בין 100 ל-1000
set random_number (math (random 100) + 100)
echo $random_number

# יצירת מספר אקראי עם 3 ספרות בין 100 ל-999
set random_number (math (random 999 - 100) / 10 + 100)
echo $random_number
```

כדי לקבל עוד פרטים ותיעוד על מכשירי המספרים האקראיים שזמינים בפיש של, ניתן להציץ ב[המדריך הרשמי של פיש](https://fishshell.com/docs/current/index.html).

## כיוון מעמיק

המכשירים המספקים מספרים אקראיים נעזרים בזמן האמת כדי ליצור סדרות של ארבעה מספרים אקראיים. כל מכשיר נעזר בזמן האמת של המערכת, שהוא חלק קטן מהזמן שחלף מאז הפעלת המכשיר הקודם. עם זאת, ישנן מכשירים הנעזרים בזמני אימוץ אחרים, כגון זמנים קבועים או פונקציות הידרונמיות. כל אלה מריצים כמו מכשירים רק בתוך המכשיר הפיזי עצמו, תוך כדי צריכת מעט המשאבים.

## ראה גם

- [תיעוד הפיש הרשמי](https://fishshell.com/docs/current/index.html)
- [מדריך להתחלת הפיש](https://fishshell.com/docs/current/tutorial.html)
- [ת