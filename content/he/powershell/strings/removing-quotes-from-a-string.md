---
date: 2024-01-26 03:42:35.081287-07:00
description: "\u05D4\u05E1\u05E8\u05EA \u05DE\u05E8\u05DB\u05D0\u05D5\u05EA \u05DE\
  \u05DE\u05D7\u05E8\u05D5\u05D6\u05EA \u05D1-PowerShell \u05DE\u05E1\u05D9\u05E8\u05D4\
  \ \u05DE\u05E8\u05DB\u05D0\u05D5\u05EA \u05D9\u05D7\u05D9\u05D3\u05D5\u05EA (`'`)\
  \ \u05D0\u05D5 \u05DB\u05E4\u05D5\u05DC\u05D5\u05EA (`\"`) \u05E9\u05DE\u05E7\u05D9\
  \u05E4\u05D5\u05EA \u05D0\u05EA \u05D4\u05D8\u05E7\u05E1\u05D8 \u05E9\u05DC\u05DA\
  . \u05EA\u05DB\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05DC\u05E2\u05D9\u05EA\u05D9\
  \u05DD \u05E7\u05E8\u05D5\u05D1\u05D5\u05EA \u05E6\u05E8\u05D9\u05DB\u05D9\u05DD\
  \ \u05DC\u05E0\u05E7\u05D5\u05EA \u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA \u05DC\
  \u05E6\u05D5\u05E8\u05DA \u05E2\u05D9\u05D1\u05D5\u05D3,\u2026"
lastmod: '2024-03-13T22:44:39.675306-06:00'
model: gpt-4-0125-preview
summary: "\u05D4\u05E1\u05E8\u05EA \u05DE\u05E8\u05DB\u05D0\u05D5\u05EA \u05DE\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA \u05D1-PowerShell \u05DE\u05E1\u05D9\u05E8\u05D4\
  \ \u05DE\u05E8\u05DB\u05D0\u05D5\u05EA \u05D9\u05D7\u05D9\u05D3\u05D5\u05EA (`'`)\
  \ \u05D0\u05D5 \u05DB\u05E4\u05D5\u05DC\u05D5\u05EA (`\"`) \u05E9\u05DE\u05E7\u05D9\
  \u05E4\u05D5\u05EA \u05D0\u05EA \u05D4\u05D8\u05E7\u05E1\u05D8 \u05E9\u05DC\u05DA\
  ."
title: "\u05D4\u05E1\u05E8\u05EA \u05DE\u05E8\u05DB\u05D0\u05D5\u05EA \u05DE\u05DE\
  \u05D7\u05E8\u05D5\u05D6\u05EA"
weight: 9
---

## מה ולמה?
הסרת מרכאות ממחרוזת ב-PowerShell מסירה מרכאות יחידות (`'`) או כפולות (`"`) שמקיפות את הטקסט שלך. תכניתנים לעיתים קרובות צריכים לנקות מחרוזות לצורך עיבוד, השוואה, או מטרות פלט, במיוחד כאשר מתמודדים עם קלט משתמש או פרסור קבצים.

## איך לעשות:
אתה יכול להשתמש באופרטור `-replace` כדי להסיר מרכאות ממחרוזת. הנה איך:

```PowerShell
# החלפת מרכאות יחידות
$stringWithSingleQuotes = "'שלום, עולם!'"
$cleanString = $stringWithSingleQuotes -replace "'", ""
Write-Output $cleanString  # פלט: שלום, עולם!

# החלפת מרכאות כפולות
$stringWithDoubleQuotes = '"שלום, עולם!"'
$cleanString = $stringWithDoubleQuotes -replace '"', ""
Write-Output $cleanString  # פלט: שלום, עולם!
```

לשני הסוגים:

```PowerShell
$stringWithQuotes = '"היי שם," היא אמרה.'
$cleanString = $stringWithQuotes -replace "[\"']", ""  # שימו לב לשימוש במחלקת תווים של regex
Write-Output $cleanString  # פלט: היי שם, היא אמרה.
```

פלט לדוגמה מהקונסול ייראה כך:

```
שלום, עולם!
שלום, עולם!
היי שם, היא אמרה.
```

## צלילה עמוקה
בימים שלפני ש-PowerShell היה רק רעיון בעיניי מיקרוסופט, עיבוד טקסט בחלונות לעיתים קרובות היה תחום של סקריפטים אצוותיים שהיו בעלי יכולות מוגבלות. הצגת PowerShell הביאה עימה יכולות חזקות למניפולציה של מחרוזות שהפך את הסקריפטינג להרבה יותר עצמתי.

קיימות חלופות ל-`-replace`, כמו להשתמש בשיטת ה-.Trim()‏ כדי להסיר מרכאות רק בהתחלה ובסוף של מחרוזת, אבל הן לא מציעות את אותה השליטה או תמיכה ב-regex.

```PowerShell
# שימוש ב-.Trim() למרכאות בהתחלה ובסוף
$stringWithQuotes = '"שלום, עולם!"'
$cleanString = $stringWithQuotes.Trim('"')
Write-Output $cleanString  # פלט: שלום, עולם!
```

שימו לב, `-replace` משתמש ב-regex מאחורי הקלעים, אז כאשר אתם עובדים איתו, זכרו שתווים מיוחדים צריכים להימלט אם אתם מכוונים אליהם. אם אתם זקוקים לשליטה יותר דקדקנית על הסרת המרכאות, צלילה לעומק ב-regex עם `-replace` היא הדרך ללכת, תוך הענקת גמישות עצומה לכם.

## ראו גם
- למידע נוסף על regex ב-PowerShell, בדקו את התיעוד הרשמי: [about_Regular_Expressions](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_regular_expressions?view=powershell-7.1)
- גלו שיטות נוספות למחרוזת: [Trim(), TrimStart(), TrimEnd()](https://docs.microsoft.com/en-us/dotnet/api/system.string.trim?view=net-6.0)
