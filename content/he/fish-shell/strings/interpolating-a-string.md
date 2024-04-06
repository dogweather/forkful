---
date: 2024-01-20 17:50:47.806901-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D0\u05D9\u05E0\
  \u05D8\u05E8\u05E4\u05D5\u05DC\u05E6\u05D9\u05D4 \u05E9\u05DC \u05DE\u05D7\u05E8\
  \u05D5\u05D6\u05D5\u05EA \u05D4\u05D9\u05D0 \u05DB\u05DC\u05D9 \u05E9\u05D4\u05EA\
  \u05E4\u05EA\u05D7 \u05E2\u05DD \u05D4\u05E9\u05E0\u05D9\u05DD \u05D1\u05E9\u05E4\
  \u05D5\u05EA \u05EA\u05D9\u05DB\u05E0\u05D5\u05EA \u05E9\u05D5\u05E0\u05D5\u05EA\
  . \u05D1-Fish Shell, \u05D4\u05D3\u05E8\u05DA \u05E9\u05D1\u05D4 \u05D0\u05EA\u05D4\
  \ \u05DE\u05E9\u05DC\u05D1 \u05DE\u05E9\u05EA\u05E0\u05D9\u05DD \u05D1\u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05D5\u05EA \u05D4\u05D9\u05D0 \u05D9\u05E9\u05D9\u05E8\u05D4\
  \ \u05D5\u05D0\u05D9\u05E0\u05D8\u05D5\u05D0\u05D9\u05D8\u05D9\u05D1\u05D9\u05EA\
  \ -\u2026"
lastmod: '2024-04-05T21:53:41.046051-06:00'
model: gpt-4-1106-preview
summary: "\u05D0\u05D9\u05E0\u05D8\u05E8\u05E4\u05D5\u05DC\u05E6\u05D9\u05D4 \u05E9\
  \u05DC \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05D4\u05D9\u05D0 \u05DB\u05DC\
  \u05D9 \u05E9\u05D4\u05EA\u05E4\u05EA\u05D7 \u05E2\u05DD \u05D4\u05E9\u05E0\u05D9\
  \u05DD \u05D1\u05E9\u05E4\u05D5\u05EA \u05EA\u05D9\u05DB\u05E0\u05D5\u05EA \u05E9\
  \u05D5\u05E0\u05D5\u05EA."
title: "\u05E9\u05E8\u05D1\u05D5\u05D1 \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA"
weight: 8
---

## איך לעשות:
```Fish Shell
# דוגמה לאינטרפולציה של מחרוזת
set name "דורון"
echo "שלום, $name"
```

תוצאה:
```
שלום, דורון
```

בואו נשלב משהו מורכב יותר:
```Fish Shell
set item "שוקולד"
set price 5
echo "המחיר של $item הוא $price שקלים"
```

תוצאה:
```
המחיר של שוקולד הוא 5 שקלים
```

## עומק ים
אינטרפולציה של מחרוזות היא כלי שהתפתח עם השנים בשפות תיכנות שונות. ב-Fish Shell, הדרך שבה אתה משלב משתנים במחרוזות היא ישירה ואינטואיטיבית - פשוט מציב את שם המשתנה בתוך גרשיים.

יש שפות אחרות, כמו PHP וPerl, שבהן זה נעשה אחרת, אבל ב-Fish זה פשוט וללא כאב ראש. מבחינת ביצועים, Fish מטפל באינטרפולציה באופן אפקטיבי. לא צריך לדאוג שהשימוש במשתנים במחרוזת יאט את הסקריפט שלך.

## ראו גם
- הדוקומנטציה הרשמית של Fish Shell על מחרוזות וגרשים: [https://fishshell.com/docs/current/index.html#syntax-string](https://fishshell.com/docs/current/index.html#syntax-string)
- מדריך מבוא ל-Fish Shell למתחילים: [https://fishshell.com/docs/current/tutorial.html](https://fishshell.com/docs/current/tutorial.html)
