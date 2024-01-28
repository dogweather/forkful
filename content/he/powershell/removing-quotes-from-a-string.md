---
title:                "הסרת מרכאות ממחרוזת"
date:                  2024-01-26T03:42:35.081287-07:00
model:                 gpt-4-0125-preview
simple_title:         "הסרת מרכאות ממחרוזת"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

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
