---
title:                "עבודה עם מספרים מרוכבים"
date:                  2024-01-26T04:44:41.172809-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם מספרים מרוכבים"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/working-with-complex-numbers.md"
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
