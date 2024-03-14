---
date: 2024-01-26 04:44:41.172809-07:00
description: "\u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05DE\u05E8\u05D5\u05DB\u05D1\u05D9\
  \u05DD, \u05D0\u05DC\u05D4 \u05E2\u05DD \u05D7\u05DC\u05E7 \u05DE\u05DE\u05E9\u05D9\
  \ \u05D5\u05D7\u05DC\u05E7 \u05DE\u05D3\u05D5\u05DE\u05D4 (\u05DB\u05DE\u05D5 3\
  \ + 4i), \u05D4\u05DD \u05D7\u05D9\u05D5\u05E0\u05D9\u05D9\u05DD \u05D1\u05EA\u05D7\
  \u05D5\u05DE\u05D9\u05DD \u05DB\u05DE\u05D5 \u05D4\u05E0\u05D3\u05E1\u05D4, \u05E4\
  \u05D9\u05D6\u05D9\u05E7\u05D4, \u05D5\u05DE\u05D3\u05E2\u05D9 \u05D4\u05E0\u05EA\
  \u05D5\u05E0\u05D9\u05DD. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\
  \u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\u05D4\u05DD \u05DC\u05E1\u05D9\u05DE\u05D5\
  \u05DC\u05E6\u05D9\u05D5\u05EA, \u05E2\u05D9\u05D1\u05D5\u05D3 \u05D0\u05D5\u05EA\
  \u05D5\u05EA,\u2026"
lastmod: '2024-03-13T22:44:39.685140-06:00'
model: gpt-4-0125-preview
summary: "\u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05DE\u05E8\u05D5\u05DB\u05D1\u05D9\
  \u05DD, \u05D0\u05DC\u05D4 \u05E2\u05DD \u05D7\u05DC\u05E7 \u05DE\u05DE\u05E9\u05D9\
  \ \u05D5\u05D7\u05DC\u05E7 \u05DE\u05D3\u05D5\u05DE\u05D4 (\u05DB\u05DE\u05D5 3\
  \ + 4i), \u05D4\u05DD \u05D7\u05D9\u05D5\u05E0\u05D9\u05D9\u05DD \u05D1\u05EA\u05D7\
  \u05D5\u05DE\u05D9\u05DD \u05DB\u05DE\u05D5 \u05D4\u05E0\u05D3\u05E1\u05D4, \u05E4\
  \u05D9\u05D6\u05D9\u05E7\u05D4, \u05D5\u05DE\u05D3\u05E2\u05D9 \u05D4\u05E0\u05EA\
  \u05D5\u05E0\u05D9\u05DD. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\
  \u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\u05D4\u05DD \u05DC\u05E1\u05D9\u05DE\u05D5\
  \u05DC\u05E6\u05D9\u05D5\u05EA, \u05E2\u05D9\u05D1\u05D5\u05D3 \u05D0\u05D5\u05EA\
  \u05D5\u05EA,\u2026"
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD \u05DE\u05E1\u05E4\u05E8\u05D9\
  \u05DD \u05DE\u05E8\u05D5\u05DB\u05D1\u05D9\u05DD"
---

{{< edit_this_page >}}

## מה ולמה?
מספרים מרוכבים, אלה עם חלק ממשי וחלק מדומה (כמו 3 + 4i), הם חיוניים בתחומים כמו הנדסה, פיזיקה, ומדעי הנתונים. מתכנתים משתמשים בהם לסימולציות, עיבוד אותות, ופתרון סוגים מסוימים של בעיות מתמטיות.

## כיצד ל:
PowerShell אינו תומך מובנה במספרים מרוכבים, כך שאתה יכול ליצור פתרון משלך או להשתמש ב-`System.Numerics.Complex` של .NET.

```PowerShell
# בואו ניצור מספרים מרוכבים באמצעות .NET
[Reflection.Assembly]::LoadWithPartialName("System.Numerics") | Out-Null

# יצירת מספרים מרוכבים
$complex1 = [System.Numerics.Complex]::new(3, 4) # 3 + 4i
$complex2 = [System.Numerics.Complex]::new(1, 2) # 1 + 2i

# חיבור שני מספרים מרוכבים
$sum = [System.Numerics.Complex]::Add($complex1, $complex2) # 4 + 6i

# כפל שני מספרים מרוכבים
$product = [System.Numerics.Complex]::Multiply($complex1, $complex2) # -5 + 10i

# הצגת התוצאות
"Sum: $sum"
"Product: $product"
```
פלט:
```
Sum: (4, 6)
Product: (-5, 10)
```

## צלילה לעומק
מספרים מרוכבים פותחו במאה ה-16 כדי לפתור משוואות שלא היו להן פתרונות בתחום המספרים הממשיים. כיום הם אבן פינה של המתמטיקה המודרנית.

התלות של PowerShell ב-.NET עבור תמיכה במספרים מרוכבים מבטיחה ביצועים טובים. אלטרנטיבות כוללות ספריות של צד שלישי או שפות תכנות אחרות כמו Python, שם מספרים מרוכבים הם סוג נתוני יליד.

## ראה גם
- [מבנה System.Numerics.Complex](https://docs.microsoft.com/en-us/dotnet/api/system.numerics.complex)
- [אריתמטיקה של מספרים מרוכבים ב-Python](https://docs.python.org/3/library/cmath.html)
