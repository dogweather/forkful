---
date: 2024-01-20 17:58:19.777377-07:00
description: "\u05D7\u05D9\u05E4\u05D5\u05E9 \u05D5\u05D4\u05D7\u05DC\u05E4\u05EA\
  \ \u05D8\u05E7\u05E1\u05D8 \u05D4\u05DD \u05E4\u05E2\u05D5\u05DC\u05D5\u05EA \u05E9\
  \u05D1\u05D4\u05DF \u05D0\u05E0\u05D5 \u05DE\u05D7\u05E4\u05E9\u05D9\u05DD \u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA \u05DE\u05E1\u05D5\u05D9\u05DE\u05EA \u05D1\u05E7\
  \u05D5\u05D3 \u05D5\u05DE\u05D7\u05DC\u05D9\u05E4\u05D9\u05DD \u05D0\u05D5\u05EA\
  \u05D4 \u05D1\u05D0\u05D7\u05E8\u05EA. \u05EA\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\
  \u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\
  \u05EA\u05E7\u05DF \u05E9\u05D2\u05D9\u05D0\u05D5\u05EA, \u05DC\u05E2\u05D3\u05DB\
  \u05DF \u05E7\u05D5\u05D3 \u05D0\u05D5 \u05DC\u05E9\u05E4\u05E8 \u05D0\u05D5\u05D8\
  \u05D5\u05DE\u05E6\u05D9\u05D4."
lastmod: '2024-03-13T22:44:40.019988-06:00'
model: gpt-4-1106-preview
summary: "\u05D7\u05D9\u05E4\u05D5\u05E9 \u05D5\u05D4\u05D7\u05DC\u05E4\u05EA \u05D8\
  \u05E7\u05E1\u05D8 \u05D4\u05DD \u05E4\u05E2\u05D5\u05DC\u05D5\u05EA \u05E9\u05D1\
  \u05D4\u05DF \u05D0\u05E0\u05D5 \u05DE\u05D7\u05E4\u05E9\u05D9\u05DD \u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05EA \u05DE\u05E1\u05D5\u05D9\u05DE\u05EA \u05D1\u05E7\u05D5\
  \u05D3 \u05D5\u05DE\u05D7\u05DC\u05D9\u05E4\u05D9\u05DD \u05D0\u05D5\u05EA\u05D4\
  \ \u05D1\u05D0\u05D7\u05E8\u05EA. \u05EA\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD\
  \ \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05EA\
  \u05E7\u05DF \u05E9\u05D2\u05D9\u05D0\u05D5\u05EA, \u05DC\u05E2\u05D3\u05DB\u05DF\
  \ \u05E7\u05D5\u05D3 \u05D0\u05D5 \u05DC\u05E9\u05E4\u05E8 \u05D0\u05D5\u05D8\u05D5\
  \u05DE\u05E6\u05D9\u05D4."
title: "\u05D7\u05D9\u05E4\u05D5\u05E9 \u05D5\u05D4\u05D7\u05DC\u05E4\u05EA \u05D8\
  \u05E7\u05E1\u05D8"
weight: 10
---

## מה ולמה?
חיפוש והחלפת טקסט הם פעולות שבהן אנו מחפשים מחרוזת מסוימת בקוד ומחליפים אותה באחרת. תכניתנים עושים זאת כדי לתקן שגיאות, לעדכן קוד או לשפר אוטומציה.

## איך לעשות:
ב-Fish Shell, שימוש ב-sed או בפקודות regex קל ויעיל. ניהול פשוט של חיפוש והחלפה ייראה כך:

```Fish Shell
echo "תוכניתנים אוהבים לקודד" | sed 's/לקודד/לתכנת/'
```

פלט דוגמא:

```
תוכניתנים אוהבים לתכנת
```

להחלפה בתוך קובץ, אתה עשוי לרצות להשתמש ב-`sed -i`:

```Fish Shell
sed -i 's/חתול/כלב/' סיפור.txt
```

זה מחליף את כל ההופעות של "חתול" ב"כלב" בקובץ `סיפור.txt`.

## עיון מעמיק
חיפוש והחלפה הם יסודות בכל שפת תכנות וסביבת עבודה. פעולה זו מקדמת אתימות של תחזוקת קוד ומאפשרת לנהל גרסאות רבות עם פחות טעויות. לעיתים נעשה שימוש בביטויים רגולריים כדי לבצע משימות מורכבות יותר של חיפוש והחלפה, מה שמצריך הבנה של syntax מתקדמת.

הדוגמאות לעיל הם רק חלק מהאפשרויות. בעזרת `grep`, `awk`, ו`perl`, תוכל לבצע חיפוש והחלפה עם יותר אופציות ודיוק.

## ראה גם
- [Documentation for sed](https://www.gnu.org/software/sed/manual/sed.html)
- [Introduction to grep](https://www.gnu.org/software/grep/manual/grep.html)
- [Using awk in shell](https://www.gnu.org/software/gawk/manual/gawk.html)
- [Perl regex documentation](https://perldoc.perl.org/perlre.html)
