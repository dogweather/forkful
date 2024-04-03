---
date: 2024-01-26 04:45:20.057506-07:00
description: "\u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05DE\u05E8\u05D5\u05DB\u05D1\u05D9\
  \u05DD \u05D4\u05DD \u05E7\u05D1\u05D5\u05E6\u05D4 \u05E9\u05DC \u05DE\u05E1\u05E4\
  \u05E8\u05D9\u05DD \u05D1\u05E6\u05D5\u05E8\u05D4 `a + bi`, \u05DB\u05D0\u05E9\u05E8\
  \ `a` \u05D5-`b` \u05D4\u05DD \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05DE\u05DE\u05E9\
  \u05D9\u05D9\u05DD, \u05D5-`i` \u05D4\u05D5\u05D0 \u05D4\u05D9\u05D7\u05D9\u05D3\
  \u05D4 \u05D4\u05DE\u05D3\u05D5\u05DE\u05D4 (`i^2 = -1`). \u05D1\u05EA\u05DB\u05E0\
  \u05D5\u05EA, \u05D0\u05E0\u05D5 \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\
  \u05D4\u05DD \u05DB\u05D3\u05D9 \u05DC\u05E4\u05EA\u05D5\u05E8\u2026"
lastmod: '2024-03-13T22:44:38.625824-06:00'
model: gpt-4-0125-preview
summary: "\u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05DE\u05E8\u05D5\u05DB\u05D1\u05D9\
  \u05DD \u05D4\u05DD \u05E7\u05D1\u05D5\u05E6\u05D4 \u05E9\u05DC \u05DE\u05E1\u05E4\
  \u05E8\u05D9\u05DD \u05D1\u05E6\u05D5\u05E8\u05D4 `a + bi`, \u05DB\u05D0\u05E9\u05E8\
  \ `a` \u05D5-`b` \u05D4\u05DD \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05DE\u05DE\u05E9\
  \u05D9\u05D9\u05DD, \u05D5-`i` \u05D4\u05D5\u05D0 \u05D4\u05D9\u05D7\u05D9\u05D3\
  \u05D4 \u05D4\u05DE\u05D3\u05D5\u05DE\u05D4 (`i^2 = -1`)."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD \u05DE\u05E1\u05E4\u05E8\u05D9\
  \u05DD \u05DE\u05E8\u05D5\u05DB\u05D1\u05D9\u05DD"
weight: 14
---

## איך לעשות:
פייתון תומכת מובנית במספרים מרוכבים. הנה איך אפשר לשחק איתם:

```Python
# יצירת מספרים מרוכבים
z = 4 + 5j
print(z)  # פלט: (4+5j)

# גישה לחלקים הממשי והמדומה
print(z.real)  # פלט: 4.0
print(z.imag)  # פלט: 5.0

# חשבון מורכב
w = 1 - 2j
print(z + w)  # פלט: (5+3j)
print(z - w)  # פלט: (3+7j)
print(z * w)  # פלט: (14+2j)
print(z / w)  # פלט: (-3.6+1.2j)

# מודולו (ערך מוחלט)
print(abs(z))  # פלט: 6.4031242374328485

# המצורע של מספר מרוכב
print(z.conjugate())  # פלט: (4-5j)
```

## צלילה עמוקה
מספרים מרוכבים הופיעו לראשונה במושגים של Gerolamo Cardano במאה ה-16. פייתון, בין שפות תכנות אחרות, מתייחסת למספרים מרוכבים כאזרחים דרג ראשון. זה אומר שהם מוטמעים בשפה, עם יכולות נוחות לשימוש, ומתחמקים מהצורך לייבא ספריות חיצוניות לפעולות בסיסיות.

עם זאת, לחישובים מספריים כבדים, יש לפייתון ספרייה בשם `cmath`, שהיא מיוחדת למספרים מרוכבים. יש בה פונקציות נוספות כמו `exp`, `log` ופעולות טריגונומטריות.

כאשר פייתון לא מספיק, ייתכן שתפנו לספריות כמו NumPy, במיוחד לפעולות מערך שכוללות מספרים מרוכבים. NumPy מספקת פעולות מאופטמות ווקטורליות שהן קריטיות לביצועים בחישוב נומרי.

## ראה גם
בדוק את המשאבים האלו ללמידה נוספת:

- תיעוד רשמי של פייתון על מספרים מרוכבים: https://docs.python.org/3/library/stdtypes.html#typesnumeric
- התיעוד של מודול `cmath`: https://docs.python.org/3/library/cmath.html
- NumPy לטיפול במערכים של מספרים מרוכבים: https://numpy.org/doc/stable/user/absolute_beginners.html#the-basics
