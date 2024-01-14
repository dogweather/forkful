---
title:                "Fish Shell: הדפסת פלט ניתוח שגיאות"
programming_language: "Fish Shell"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/fish-shell/printing-debug-output.md"
---

{{< edit_this_page >}}

## למה

לבדוק את תקינות הקוד שנכתב לצורך תיקון בעיות והשגת תוצאות נכונות.

## כיצד לעשות זאת

אם את/ה משתמש/ת בפקודת השקולט (echo), תוכל/י להדפיס כל משתנה, מחרוזת או ערך אחר שתרצה לבדוק. לדוגמה:

```
fish shell
set my_var "זהו משתנה לדוגמה"
echo $my_var
```

זה יחזיר את הפלט הבא:

```
זהו משתנה לדוגמה
```

בנוסף, ניתן להשתמש בפקודת דיבאג (debug) להדפסת ערכים ומשתנים בזמן ריצה של התוכנית. לדוגמה:

```
fish shell
set var1 "ערך ראשון"
set var2 "ערך שני"
debug $var1 $var2
```

הפלט ייראה כך:

```
משתנה: ״var1משתנה זה מכיל את ערך״ ערך ראשון
משתנה: ״var2משתנה זה מכיל אתערך״ ערך שני
```

## ירידה לעומק

הדפסת פלט דיבאג יכולה לעזור למצוא את המקור לבעיות בקוד ולתקן אותן בקלות. כמו כן, היא עשויה להראות את הפעילות של משתנים וביטויים בזמן הריצה של הקוד.

בנוסף, ניתן להשתמש בהדפסת פלט כדי לבדוק תנאים ולבדוק שהם מתקיימים בעת הרצת התוכנית.

## ראה גם

למידע נוסף על פקודות Fish Shell, ראה את הקישורים הבאים:

- https://fishshell.com/docs/current/tutorial.html
- https://fishshell.com/docs/current/index.html
- https://fishshell.com/docs/current/faq.html