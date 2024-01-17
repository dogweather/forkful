---
title:                "חישוב תאריך בעתיד או בעבר"
html_title:           "PowerShell: חישוב תאריך בעתיד או בעבר"
simple_title:         "חישוב תאריך בעתיד או בעבר"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## מה ולמה?

חישוב תאריך בעתיד או בעבר הוא פעולה שמאפשרת לנו לקבל תאריך חדש מתוך תאריך קיים. פעולה זו חשובה לפיתוחנו כמתכנתים, כי היא מאפשרת לנו לעשות חישובים וליצור תאריך מתאים בקלות ובמהירות.

## כיצד לעשות:

תוכן הכתבה יתבסס על דוגמאות של קוד פשוטות בשפת PowerShell עם תוצאות פלט מתאימות. כל דוגמה ייתקלו בתוך מתח קוד PowerShell לצורך ייחודיות וזיהוי ברור שלהן.

### חישוב תאריך מסויים בעבר או בעתיד:

```PowerShell
# דוגמה לחישוב תאריך עתידי באמצעות הפרמטר -Days
# בדוגמה זו, אנו מחשבים תאריך משנה עתידית מהתאריך הנוכחי

(Get-Date).AddDays(365)

# Output:
# Sunday, June 06, 2022 4:09:56 PM
```

```PowerShell
# דוגמה לחישוב תאריך מסויים בעבר באמצעות הפרמטר -Months
# בדוגמה זו, אנו מחשבים תאריך לפני שנתיים מהתאריך הנוכחי

(Get-Date).AddMonths(-24)

# Output:
# Monday, June 06, 2019 4:09:56 PM
```

### חישוב תאריך עם פרמטרים נוספים:

```PowerShell
# דוגמה לחישוב תאריך עתידי באמצעות הפרמטרים -Years, -Months, -Days
# בדוגמה זו, אנו מחשבים תאריך מתוך תאריך מסויים שנקבע על ידי הפרמטרים

(Get-Date -Year 2020 -Month 5 -Day 17).AddYears(2).AddMonths(2).AddDays(5)

# Output:
# Monday, July 22, 2024 12:00:00 AM
```

## צלילת עומק:

חישוב תאריך בעתיד או בעבר הוא נושא נפוץ בתכנות וניתן למצוא מגוון של פתרונות עבורו. ב- PowerShell, קיימים פקודות מובנות כמו Get-Date ו-AddYears שמאפשרים לנו לעבד תאריכים בקלות ובמהירות. כמו כן, קיימים פקודות שלישיות וספריות מתוכננות כמו ספריית NCalc שמציעה תכונות נוספות כמו חישוב תאריכים לפי תבניות מוגדרות על ידי המשתמש.

## ראו גם:

לקבלת מידע נוסף על פקודות וספריות המאפשרות חישוב תאריכים בעתיד או בעבר, ניתן לעיין במקורות הבאים:

- פיתוח תכנות בכפוף: תאריך ושעה ב- PowerShell - https://docs.microsoft.com/en-us/dotnet/standard/base-types/datetime
- ספריית NCalc - https://ncalc.codeplex.com/
- פקודת Get-Date בחוברת פקודות של Microsoft - https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.utility/get-date