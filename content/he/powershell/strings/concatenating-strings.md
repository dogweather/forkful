---
date: 2024-01-20 17:35:54.334375-07:00
description: "\u05D4\u05D3\u05D1\u05E7\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA\
  \ (Concatenating strings) \u05D4\u05D9\u05D0 \u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\
  \u05D1\u05D5 \u05D0\u05E0\u05D5 \u05E7\u05D5\u05D1\u05E2\u05D9\u05DD \u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05D5\u05EA \u05DC\u05D9\u05D3 \u05D6\u05D5 \u05DC\u05E6\u05D5\
  \u05E8\u05DA \u05D9\u05E6\u05D9\u05E8\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA\
  \ \u05D7\u05D3\u05E9\u05D4. \u05EA\u05D5\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD\
  \ \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D1\
  \u05E0\u05D5\u05EA \u05D8\u05E7\u05E1\u05D8\u05D9\u05DD \u05D3\u05D9\u05E0\u05DE\
  \u05D9\u05D9\u05DD \u05D0\u05D5\u2026"
lastmod: '2024-03-13T22:44:39.681770-06:00'
model: gpt-4-1106-preview
summary: "\u05D4\u05D3\u05D1\u05E7\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA\
  \ (Concatenating strings) \u05D4\u05D9\u05D0 \u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\
  \u05D1\u05D5 \u05D0\u05E0\u05D5 \u05E7\u05D5\u05D1\u05E2\u05D9\u05DD \u05DE\u05D7\
  \u05E8\u05D5\u05D6\u05D5\u05EA \u05DC\u05D9\u05D3 \u05D6\u05D5 \u05DC\u05E6\u05D5\
  \u05E8\u05DA \u05D9\u05E6\u05D9\u05E8\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA\
  \ \u05D7\u05D3\u05E9\u05D4. \u05EA\u05D5\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD\
  \ \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D1\
  \u05E0\u05D5\u05EA \u05D8\u05E7\u05E1\u05D8\u05D9\u05DD \u05D3\u05D9\u05E0\u05DE\
  \u05D9\u05D9\u05DD \u05D0\u05D5\u2026"
title: "\u05E9\u05E8\u05E9\u05D5\u05E8 \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA"
---

{{< edit_this_page >}}

## מה ולמה?
הדבקת מחרוזות (Concatenating strings) היא תהליך שבו אנו קובעים מחרוזות ליד זו לצורך יצירת מחרוזת חדשה. תוכניתנים עושים זאת כדי לבנות טקסטים דינמיים או לשלב מידע ממקורות שונים.

## איך לעשות:
הדבקה (concatenation) ב-PowerShell פשוטה. הנה דוגמאות:

```PowerShell
# דוגמה 1 - באמצעות פלוס (+)
$string1 = 'שלום'
$string2 = 'עולם'
$greeting = $string1 + ' ' + $string2
$greeting  # ידפיס 'שלום עולם'

# דוגמה 2 - באמצעות תבניות (templates)
$name = 'דן'
$welcomeMessage = "ברוך הבא, $name!"
$welcomeMessage  # ידפיס 'ברוך הבא, דן!'
```

## צלילה עמוקה
בעבר, בשפות כמו C, הדבקת מחרוזות הייתה מורכבת יותר ודרשה פונקציות מיוחדות כמו `strcat()`. ב-PowerShell, זה פשוט יותר וכולל אפשרויות כמו אופרטור הפלוס, שימוש בתבניות מחרוזות והתמרה אוטומטית של טיפוסים. יתר על כן, הדבקה יכולה להתבצע גם עם עזרת הפונקציה `-f`, אשר מספקת יכולת התאמה עוד גבוהה יותר ופורמט מדויק.

אלטרנטיבה נוספת היא השימוש בפונקציה `Join-String`, המאפשרת ביצוע הדבקת מחרוזות באופן מיטבי עבור קולקציות של נתונים.


## ראה גם
- [תיעוד הרשמי של Microsoft לפונקציה Join-String](https://learn.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/join-string?view=powershell-7.1)
- [גישות להדבקת מחרוזות ב-PowerShell באתר StackOverflow](https://stackoverflow.com/questions/27175137/powershell-concatenating-strings)
