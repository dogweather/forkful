---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:07:44.375977-07:00
description: "\u05D1\u05D3\u05D9\u05E7\u05D4 \u05D4\u05D0\u05DD \u05E1\u05E4\u05E8\
  \u05D9\u05D9\u05D4 \u05E7\u05D9\u05D9\u05DE\u05EA \u05D1-Fish Shell \u05DE\u05D0\
  \u05E4\u05E9\u05E8\u05EA \u05DC\u05E1\u05E7\u05E8\u05D9\u05E4\u05D8\u05D9\u05DD\
  \ \u05DC\u05E7\u05D1\u05DC \u05D4\u05D7\u05DC\u05D8\u05D5\u05EA \u05D1\u05D4\u05EA\
  \u05D1\u05E1\u05E1 \u05E2\u05DC \u05D4\u05E0\u05D5\u05DB\u05D7\u05D5\u05EA \u05D0\
  \u05D5 \u05D4\u05D9\u05E2\u05D3\u05E8 \u05E9\u05DC \u05DE\u05D1\u05E0\u05D9 \u05EA\
  \u05D9\u05E7\u05D9\u05D5\u05EA, \u05D3\u05D1\u05E8 \u05D4\u05DE\u05D0\u05E4\u05E9\
  \u05E8 \u05D1\u05D9\u05E6\u05D5\u05E2 \u05DE\u05E9\u05D9\u05DE\u05D5\u05EA \u05DB\
  \u05DE\u05D5 \u05E4\u05E2\u05D5\u05DC\u05D5\u05EA \u05E7\u05D1\u05E6\u05D9\u05DD\
  \u2026"
lastmod: '2024-03-13T22:44:40.074031-06:00'
model: gpt-4-0125-preview
summary: "\u05D1\u05D3\u05D9\u05E7\u05D4 \u05D4\u05D0\u05DD \u05E1\u05E4\u05E8\u05D9\
  \u05D9\u05D4 \u05E7\u05D9\u05D9\u05DE\u05EA \u05D1-Fish Shell \u05DE\u05D0\u05E4\
  \u05E9\u05E8\u05EA \u05DC\u05E1\u05E7\u05E8\u05D9\u05E4\u05D8\u05D9\u05DD \u05DC\
  \u05E7\u05D1\u05DC \u05D4\u05D7\u05DC\u05D8\u05D5\u05EA \u05D1\u05D4\u05EA\u05D1\
  \u05E1\u05E1 \u05E2\u05DC \u05D4\u05E0\u05D5\u05DB\u05D7\u05D5\u05EA \u05D0\u05D5\
  \ \u05D4\u05D9\u05E2\u05D3\u05E8 \u05E9\u05DC \u05DE\u05D1\u05E0\u05D9 \u05EA\u05D9\
  \u05E7\u05D9\u05D5\u05EA, \u05D3\u05D1\u05E8 \u05D4\u05DE\u05D0\u05E4\u05E9\u05E8\
  \ \u05D1\u05D9\u05E6\u05D5\u05E2 \u05DE\u05E9\u05D9\u05DE\u05D5\u05EA \u05DB\u05DE\
  \u05D5 \u05E4\u05E2\u05D5\u05DC\u05D5\u05EA \u05E7\u05D1\u05E6\u05D9\u05DD\u2026"
title: "\u05D1\u05D3\u05D9\u05E7\u05D4 \u05D0\u05DD \u05E1\u05E4\u05E8\u05D9\u05D9\
  \u05D4 \u05E7\u05D9\u05D9\u05DE\u05EA"
weight: 20
---

## מה ולמה?
בדיקה האם ספרייה קיימת ב-Fish Shell מאפשרת לסקריפטים לקבל החלטות בהתבסס על הנוכחות או היעדר של מבני תיקיות, דבר המאפשר ביצוע משימות כמו פעולות קבצים מותנות, רישום לוגים או הקמת סביבה. טכניקה זו קריטית לכתיבת סקריפטים חזקים המתקשרים עם מערכת הקבצים בצורה צפויה.

## איך לעשות זאת:
Fish Shell משתמש בפקודת `test` כדי לבדוק סוגי קבצים ותכונותיהם, כולל אם היעד הוא ספרייה. הנה תבנית בסיסית לבדיקה אם ספרייה קיימת:

```fish
if test -d /path/to/dir
    echo "הספרייה קיימת"
else
    echo "הספרייה אינה קיימת"
end
```
פלט לדוגמה:
```
הספרייה קיימת
```

לצורך פעולות קבצים וספריות יעילות יותר, ייתכן שאחד יעבור לכלים חיצוניים כמו `fd`, עם זאת זה בדרך כלל נמצא בשימוש לחיפוש קבצים ותיקיות ולא רק לבדיקה של קיום. בכל אופן, שילוב שלו עם סקריפטים של Fish יכול לתת תוצאות שימושיות:

```fish
set dir "/path/to/search"
if fd . $dir --type directory --max-depth 1 | grep -q $dir
    echo "הספרייה קיימת"
else
    echo "הספרייה אינה קיימת"
end
```

דוגמת ה-`fd` הזו מחפשת את הספרייה בעומק נתון, ו-`grep` בודק להתאמה, מה שהופך אותה לגמישה לבדיקות מורכבות. עם זאת, למטרת הבדיקה הישירה של קיום, הסתמכות על ה-`test` המובנה של Fish היא גם יעילה וגם פשוטה.
