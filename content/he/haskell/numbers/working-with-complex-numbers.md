---
date: 2024-01-26 04:42:20.057984-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D4\u05E1\u05E7\
  \u05DC \u05DE\u05D8\u05E4\u05DC \u05D1\u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05DE\
  \u05E8\u05D5\u05DB\u05D1\u05D9\u05DD \u05E2\u05DD \u05D4\u05DE\u05D5\u05D3\u05D5\
  \u05DC `Data.Complex`. \u05D4\u05E0\u05D4 \u05E1\u05D9\u05D5\u05E8 \u05DE\u05D4\u05D9\
  \u05E8."
lastmod: '2024-03-13T22:44:39.403536-06:00'
model: gpt-4-0125-preview
summary: "\u05D4\u05E1\u05E7\u05DC \u05DE\u05D8\u05E4\u05DC \u05D1\u05DE\u05E1\u05E4\
  \u05E8\u05D9\u05DD \u05DE\u05E8\u05D5\u05DB\u05D1\u05D9\u05DD \u05E2\u05DD \u05D4\
  \u05DE\u05D5\u05D3\u05D5\u05DC `Data.Complex`."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD \u05DE\u05E1\u05E4\u05E8\u05D9\
  \u05DD \u05DE\u05E8\u05D5\u05DB\u05D1\u05D9\u05DD"
weight: 14
---

## איך לעשות:
הסקל מטפל במספרים מרוכבים עם המודול `Data.Complex`. הנה סיור מהיר:

```haskell
import Data.Complex

-- הגדרת שני מספרים מרוכבים
let z1 = 3 :+ 4  -- זה 3 + 4i
let z2 = 5 :+ (-2)  -- 5 - 2i

-- פעולות אריתמטיות
let sum = z1 + z2  -- 8 :+ 2
let difference = z1 - z2  -- -2 :+ 6
let product = z1 * z2  -- 23 :+ 14
let quotient = z1 / z2  -- 0.20689655172413793 :+ 0.9655172413793104

-- שותף מרוכב
let conjugateZ1 = conjugate z1  -- 3 :+ (-4)

-- גודל ושלב
let magnitudeZ1 = magnitude z1  -- 5.0
let phaseZ1 = phase z1  -- 0.9272952180016122

-- המרה מקוטב לריבועי ולהפך
let z1Polar = polar z1  -- (5.0,0.9272952180016122)
let fromPolar = mkPolar 5.0 0.9272952180016122  -- כמו z1
```

פלט לדוגמה אחרי טעינת הקוד הנ"ל ב-GHCi עשוי להיות:

```haskell
*Main> sum
8.0 :+ 2.0
*Main> product
23.0 :+ 14.0
*Main> magnitudeZ1
5.0
```

## צלילה עמוקה
מספרים מרוכבים חוזרים למאה ה-16 אך זכו לקבלה רחבה הרבה מאוחר יותר. הסקל, כמו שפות רבות, מספק תמיכה ילידית לאריתמטיקה מרוכבת, המקלה על העבודה עם מספרים אלה מבלי ליישם את המתמטיקה הבסיסית.

אלטרנטיבות כוללות בנייה של סוג מספר מרוכב מותאם אישית או שימוש בספריות לתחומים ספציפיים כמו קווטרניונים לגרפיקת 3D. אך לרוב תרחישי השימוש, `Data.Complex` של הסקל מספק בהחלט.

מאחורי הקלעים, `Data.Complex` הוא רק סוג נתונים המזווג שני ערכי `Float` או `Double`, המייצגים את החלקים הממשי והמדומה, בהתאמה. זוהי דרך יעילה וישירה לעבוד עם מספרים מרוכבים על פלטפורמת הסקל.

## ראו גם
בדוקו את המשאבים האלה למידע נוסף על מספרים מרוכבים בהסקל:

- התיעוד הרשמי של הסקל `Data.Complex`: [Hackage Data.Complex](https://hackage.haskell.org/package/base-4.16.1.0/docs/Data-Complex.html)
- צלילה עמוקה יותר לטיפוסי המספרים של הסקל: [Learn You a Haskell for Great Good!](http://learnyouahaskell.com/starting-out#numbers)
- ליישום, חקר אלגוריתמי טרנספורמציה פורייה מהירים בהסקל: [ספריית FFT של הסקל](https://hackage.haskell.org/package/fft)
