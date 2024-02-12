---
title:                "קבלת התאריך הנוכחי"
date:                  2024-02-03T19:10:57.399869-07:00
model:                 gpt-4-0125-preview
simple_title:         "קבלת התאריך הנוכחי"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/getting-the-current-date.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

איחזור התאריך הנוכחי ב-PowerShell קשור לשליפת תאריך ושעה נוכחית של המערכת. פעולה זו יסודית למשימות כגון רישום לוגים, קיזוז זמנים או קבלת החלטות על סמך תאריכים. מתכנתים משתמשים ביכולת זו כדי לעקוב אחר אירועים, לתזמן משימות ולטפל בלוגיקה שתלויה בתאריך בתסריטים וביישומים.

## איך לעשות:

PowerShell מספקת cmdlets פשוטות לקבלת התאריך והשעה. ה-cmdlet `Get-Date` הוא הכלי העיקרי למטרה זו. הוא יכול להחזיר את התאריך והשעה המלאים, אותם תוכלו לעצב או לתת לפי הצורך.

```powershell
# לקבל את התאריך והשעה הנוכחיים
Get-Date
```

**פלט לדוגמא:**

```
שלישי, 5 בספטמבר 2023 9:46:02 AM
```

ניתן גם לעצב את הפלט כך שיציג רק את המידע שאתם צריכים, כמו רק את התאריך או רק את השעה.

```powershell
# לקבל רק את התאריך הנוכחי בפורמט מסוים
Get-Date -Format "yyyy-MM-dd"
```

**פלט לדוגמא:**

```
2023-09-05
```

```powershell
# לקבל רק את השעה הנוכחית
Get-Date -Format "HH:mm:ss"
```

**פלט לדוגמא:**

```
09:46:02
```

### שימוש במחלקה של .NET

PowerShell מאפשרת גישה ישירה למחלקות של .NET, ומציעה דרך חלופית לעבוד עם תאריכים ושעות.

```powershell
# שימוש במחלקת DateTime של .NET לקבלת התאריך והשעה הנוכחיים
[System.DateTime]::Now
```

**פלט לדוגמא:**

```
שלישי, 5 בספטמבר 2023 9:46:02 AM
```

לזמן UTC:

```powershell
# שימוש במחלקת DateTime של .NET לקבלת התאריך והשעה הגלובלית (UTC) הנוכחיים
[System.DateTime]::UtcNow
```

**פלט לדוגמא:**

```
שלישי, 5 בספטמבר 2023 1:46:02 PM
```

פקודות ומחלקות אלה מספקות אפשרויות חזקות וגמישות לעבודה עם תאריכים ושעות ב-PowerShell, חיוניות למשימות סקריפטינג ואוטומציה רבות.