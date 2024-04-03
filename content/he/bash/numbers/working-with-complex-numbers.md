---
date: 2024-01-26 04:37:10.919987-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: Bash \u05DC\u05D0\
  \ \u05EA\u05D5\u05DE\u05DA \u05D1\u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05DE\u05E8\
  \u05D5\u05DB\u05D1\u05D9\u05DD \u05D1\u05D0\u05D5\u05E4\u05DF \u05D8\u05D1\u05E2\
  \u05D9. \u05DC\u05E2\u05D9\u05EA\u05D9\u05DD, \u05EA\u05E9\u05EA\u05DE\u05E9 \u05D1\
  \u05DB\u05DC\u05D9 \u05D7\u05D9\u05E6\u05D5\u05E0\u05D9 \u05DB\u05DE\u05D5 `bc`\
  \ \u05E2\u05DD \u05D4\u05D0\u05E4\u05E9\u05E8\u05D5\u05EA `-l` \u05E9\u05DC\u05D5\
  . \u05DB\u05DA \u05EA\u05E2\u05D1\u05D3 \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05DE\
  \u05E8\u05D5\u05DB\u05D1\u05D9\u05DD \u05D1bash."
lastmod: '2024-03-13T22:44:39.610567-06:00'
model: gpt-4-0125-preview
summary: "Bash \u05DC\u05D0 \u05EA\u05D5\u05DE\u05DA \u05D1\u05DE\u05E1\u05E4\u05E8\
  \u05D9\u05DD \u05DE\u05E8\u05D5\u05DB\u05D1\u05D9\u05DD \u05D1\u05D0\u05D5\u05E4\
  \u05DF \u05D8\u05D1\u05E2\u05D9."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD \u05DE\u05E1\u05E4\u05E8\u05D9\
  \u05DD \u05DE\u05E8\u05D5\u05DB\u05D1\u05D9\u05DD"
weight: 14
---

## איך לעשות:
Bash לא תומך במספרים מרוכבים באופן טבעי. לעיתים, תשתמש בכלי חיצוני כמו `bc` עם האפשרות `-l` שלו. כך תעבד מספרים מרוכבים בbash:

```bash
echo "sqrt(-1)" | bc -l
```

פלט:
```bash
j
```

כפל:

```bash
echo "(-1 + -1i) * (4 + 3i)" | bc -l
```

פלט:
```bash
-1.00000000000000000000-7.00000000000000000000i
```

## צלילה עמוקה
מספרים מרוכבים קיימים מהמאה ה-16, אך שפות סקריפט כמו Bash אינן מוכנות מראש לביצועי חישובים מתמטיים כמו מספרים מרוכבים ישירות מהקופסא. זו הסיבה ש-`bc` או כלים אחרים כמו `awk` לעיתים נכנסים לתמונה. כמה שפות חלופיות לעבודה עם מספרים מרוכבים הן Python עם המודול `cmath` ו-MATLAB, ששתיהן בנויות לפונקציות מתמטיות מתקדמות יותר. בשביל Bash, זה כל עניין של לנצל כלים - `bc` משתמש ב-'i' הקטן לייצוג היחידה המדומה ותומך בפעולות בסיסיות כמו חיבור, חיסור, כפל, וחילוק.

## ראו גם
- מדריך ל`bc`: https://www.gnu.org/software/bc/manual/html_mono/bc.html
- GNU Octave (חלופה ל-MATLAB): https://www.gnu.org/software/octave/
- מודול `cmath` של Python: https://docs.python.org/3/library/cmath.html
