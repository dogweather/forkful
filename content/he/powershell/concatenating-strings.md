---
title:                "שרשור מחרוזות"
date:                  2024-01-20T17:35:54.334375-07:00
model:                 gpt-4-1106-preview
simple_title:         "שרשור מחרוזות"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/concatenating-strings.md"
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
