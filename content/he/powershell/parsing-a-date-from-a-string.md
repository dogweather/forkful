---
title:                "פענוח תאריך ממחרוזת"
html_title:           "Bash: פענוח תאריך ממחרוזת"
simple_title:         "פענוח תאריך ממחרוזת"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?

עיבוד תאריך ממחרוזת הוא פעולה שבה משתמשים בקוד להמיר מחרוזת כלשהי אשר מתארת תאריך, למשתנה מסוג תאריך. השימוש בפעולה זו מוצדק כאשר נתוני תאריך מגיעים בפורמטים שונים ואנו רוצים להימנע מבעיות תאימות.

## איך:

להלן דוגמאות לקודים ב-PowerShell:

```PowerShell
# פורמט YYYY/MM/DD
$DateString = '2022/01/01'
$DateObject = [DateTime]::ParseExact($DateString, 'yyyy/MM/dd', $null)
$DateObject
```

דוגמה לפלט:
```
Saturday, January 01, 2022 12:00:00 AM
```

```PowerShell
# פורמט DD-MM-YYYY 
$DateString = '01-01-2022'
$DateObject = [DateTime]::ParseExact($DateString,'dd-MM-yyyy',$null)
$DateObject
```

דוגמה לפלט:
```
Saturday, January 01, 2022 12:00:00 AM
```

## עומק תוכן:

פעמים רבות, מידע מגיע בתצורות שונות ולא מתאים. במהלך השנים, התקנים נהיו יותר אחידים, אך לפעמים משתמשים עדיין ישלחו את התאריך במחרוזת שבהלם להמיר.

חלופות ל-PowerShell כוללות שפות אחרות כמו Python, Java, או C#, שכולם מציעים המרת מחרוזת לתאריך, אך בשיטות ותחבירים שונים.

ההמרה ב-PowerShell מתבצעת באמצעות שיטת ה-ParseExact של האובייקט DateTime. אנו מספקים את המחרוזת, הפורמט של המחרוזת, והמשתנה שמכיל את המידע.

## ראו גם:

- התיעוד הרשמי של Microsoft ל-[DateTime.ParseExact](https://docs.microsoft.com/en-us/dotnet/api/system.datetime.parseexact)
- כמה דוגמאות של [מה ניתן למר עם ParseExact](https://www.dotnetperls.com/datetime-parseexact) על dotnetperls.com.