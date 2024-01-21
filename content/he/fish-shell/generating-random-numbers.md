---
title:                "גילוי מספרים אקראיים"
date:                  2024-01-20T17:49:07.781619-07:00
model:                 gpt-4-1106-preview
simple_title:         "גילוי מספרים אקראיים"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## What & Why?
מה זה מספרים אקראיים ולמה צריך אותם? פשוט, זה מספרים שלא תוכל לנחש את הערך שלהם מראש, ואנחנו משתמשים בהם בכל מיני תרחישים, כמו ביטחון מחשבים, משחקים וסימולציות. 

## How to:
ב-Fish Shell, זה סופר פשוט ליצור מספר אקראי. איך עושים את זה? תבדוק את הדוגמאות הבאות:

```Fish Shell
# לקבל מספר אקראי בין 1 ל-10
set -l random_number (random 1 10)
echo $random_number
```

דוגמא לפלט:
```
5
```

```Fish Shell
# לקבל מספר אקראי בטווח רחב יותר, נניח 1 עד 100
set -l random_number (random 1 100)
echo $random_number
```

דוגמא לפלט:
```
73
```

## Deep Dive
בעבר, יצירת מספרים אקראיים הייתה אתגר גדול. מערכות מחשבים משתמשות באלגוריתמים מתוחכמים ליצירת מספרים שנראים אקראיים. ב-Fish Shell, הפונקציה `random` מספקת דרך נוחה לגרום לזה. תחת הכובע, היא עשויה להשתמש במקורות מהגרעין של המערכת ליצירת אנטרופיה.

יש גם אלטרנטיבות ל-Fish Shell כמו Bash או Zsh שבהן תהליך היצירה של מספרים אקראיים עשוי להיות שונה מעט.

שים לב שלמרות שהפונקציה נקראת `random`, המספרים שהיא מייצרת הם פסבדו-אקראיים - דהיינו, הם נראים אקראיים אבל נקבעים לפי אלגוריתם שאם ידועות ההנחות שלו אז ניתן לחזות את התוצאות.

## See Also
הנה כמה קישורים שיכולים לעזור לך להרחיב את הידע שלך בנושא מספרים אקראיים והשימוש בהם בקוד:

- [fish shell documentation on random](https://fishshell.com/docs/current/cmds/random.html)
- [Wikipedia on Pseudorandom number generators](https://en.wikipedia.org/wiki/Pseudorandom_number_generator)
- [StackOverflow discussion on random numbers in shell scripting](https://stackoverflow.com/questions/tagged/random+shell)