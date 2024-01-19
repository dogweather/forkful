---
title:                "השוואה בין שני תאריכים"
html_title:           "Arduino: השוואה בין שני תאריכים"
simple_title:         "השוואה בין שני תאריכים"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/comparing-two-dates.md"
---

{{< edit_this_page >}}

## מה זה & למה זה משמש?
השוואה של שני תאריכים היא פעולה שבה אנו מבדילים בין שני תאריכים כדי לראות איזה מהם בא מקודם או אם הם שווים. מתכנתים משתמשים בה כדי לנהל ולקבל מידע על סדר תאריכים וזמנים.

## איך לעשות:
```PowerShell
# יצירת שני תאריכים להשוואה
$date1 = Get-Date -Year 2021 -Month 3 -Day 5
$date2 = Get-Date -Year 2021 -Month 5 -Day 3

# השוואת התאריכים
if ($date1 -gt $date2) {
    "Date1 is later than Date2"
} elseif ($date1 -lt $date2) {
    "Date1 is earlier than Date2"
} else {
    "Date1 and Date2 are the same"
}
```
תוצאה משולשת תהיה אחת מהבאות: "Date1 is later than Date2", "Date1 is earlier than Date2" או "Date1 and Date2 are the same" בהתאם להשוואה.

## בהרחבה:
השוואת תאריכים ב-PowerShell הייתה אתגר בעבר כאשר השפה התמקדה בטקסט לחלופין של נתונים ממדים. אולם, PowerShell גדלה והתפתחה, והציבור דרש דרך רובוסטית יותר לעבוד עם תאריכים. כלים אלטרנטיביים כמו כמה שפות .NET מאפשרות השוואת תאריכים, אך פיתרון PowerShell הוא בהחלט הכי פשוט. להשראה המשך, יש לפנות למסמכי המפרט של PowerShell.

## ראו גם:
* [תיעוד PowerShell](https://docs.microsoft.com/powershell/scripting/overview)
* [.NET DateTime Class](https://docs.microsoft.com/en-us/dotnet/api/system.datetime)
* [Comparison Operators - PowerShell](https://docs.microsoft.com/powershell/module/microsoft.powershell.core/about/about_comparison_operators)