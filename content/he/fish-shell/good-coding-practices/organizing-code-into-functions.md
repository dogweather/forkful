---
changelog:
- 2024-01-28, dogweather, reviewed and added links
- 2024-01-28, gpt-4-0125-preview, translated from English
date: 2024-01-28 23:03:56.102558-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1-Fish, \u05DB\
  \u05D5\u05EA\u05D1\u05D9\u05DD \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D4 \u05E2\
  \u05DD \u05D4\u05DE\u05D9\u05DC\u05D4 \u05D4\u05E9\u05DE\u05D5\u05E8\u05D4 `function`,\
  \ \u05E0\u05D5\u05EA\u05E0\u05D9\u05DD \u05DC\u05D4 \u05E9\u05DD, \u05D5\u05DE\u05E1\
  \u05D9\u05D9\u05DE\u05D9\u05DD \u05E2\u05DD `end`. \u05D4\u05E0\u05D4 \u05E4\u05D5\
  \u05E0\u05E7\u05E6\u05D9\u05D4 \u05E4\u05E9\u05D5\u05D8\u05D4."
lastmod: '2024-03-13T22:44:40.058737-06:00'
model: gpt-4-0125-preview
summary: "\u05D1-Fish, \u05DB\u05D5\u05EA\u05D1\u05D9\u05DD \u05E4\u05D5\u05E0\u05E7\
  \u05E6\u05D9\u05D4 \u05E2\u05DD \u05D4\u05DE\u05D9\u05DC\u05D4 \u05D4\u05E9\u05DE\
  \u05D5\u05E8\u05D4 `function`, \u05E0\u05D5\u05EA\u05E0\u05D9\u05DD \u05DC\u05D4\
  \ \u05E9\u05DD, \u05D5\u05DE\u05E1\u05D9\u05D9\u05DE\u05D9\u05DD \u05E2\u05DD `end`."
title: "\u05D0\u05E8\u05D2\u05D5\u05DF \u05D4\u05E7\u05D5\u05D3 \u05DC\u05EA\u05D5\
  \u05DA \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05EA"
weight: 18
---

## איך לעשות:
ב-Fish, כותבים פונקציה עם המילה השמורה `function`, נותנים לה שם, ומסיימים עם `end`. הנה פונקציה פשוטה:

```fish
function hello
    echo "Hello, World!"
end

hello
```

פלט:
```
Hello, World!
```

כעת, בואו נעשה שהיא תברך אדם:

```fish
function greet
    set user (whoami)
    echo "Hey there, $user!"
end

greet
```

פלט:
```
Hey there, your_username!
```

כדי לשמור אותה לרוחב הפעלות, השתמש ב-`funcsave greet`.

## צלילה לעומק
פונקציות ב-Fish Shell הן כמו מיני-סקריפטים - ניתן להכניס לתוכן כמעט כל דבר. מבחינה היסטורית, המושג של פונקציות בסקריפטים של מעטפת הציל עשרות שעות של הקלדה וניפוי שגיאות חוזרות. בניגוד לשפות תכנות כמו Python, פונקציות ב-Shell יותר על נוחות מאשר על מבנה.

שפות Shell כמו Bash משתמשות ב-`function` או סוגריים ישירות. Fish נשארת עם `function ... end`— ברור וקריא. בתוך פונקציות Fish, יש לכם את כל האפשרויות: פרמטרים, משתנים מקומיים עם `set -l`, ואפילו ניתן להגדיר פונקציה בתוך פונקציה אחרת.

אין צורך בערך `return` מכיוון ש-Fish לא מתמקדת בזה; פלט הפונקציה שלכם הוא החזר שלה. ואם אתם רוצים פונקציות קבועות שתהיינה זמינות להפעלות עתידיות, זכרו את `funcsave`.

## ראו גם
- המדריך ל-fish על פונקציות: [https://fishshell.com/docs/current/tutorial.html#tut_functions](https://fishshell.com/docs/current/tutorial.html#functions)

### פקודות לפונקציה
- [function](https://fishshell.com/docs/current/cmds/function.html) — ליצור פונקציה
- [functions](https://fishshell.com/docs/current/cmds/functions.html) — להדפיס או למחוק פונקציות
- [funcsave](https://fishshell.com/docs/current/cmds/funcsave.html) — לשמור את הגדרת הפונקציה בתיקיית הטעינה האוטומטית של המשתמש
- [funced](https://fishshell.com/docs/current/cmds/funced.html) — לערוך פונקציה באופן אינטראקטיבי
