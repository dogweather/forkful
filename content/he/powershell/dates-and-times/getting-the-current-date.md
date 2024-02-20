---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:57.399869-07:00
description: "\u05D0\u05D9\u05D7\u05D6\u05D5\u05E8 \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA\
  \ \u05D4\u05E0\u05D5\u05DB\u05D7\u05D9 \u05D1-PowerShell \u05E7\u05E9\u05D5\u05E8\
  \ \u05DC\u05E9\u05DC\u05D9\u05E4\u05EA \u05EA\u05D0\u05E8\u05D9\u05DA \u05D5\u05E9\
  \u05E2\u05D4 \u05E0\u05D5\u05DB\u05D7\u05D9\u05EA \u05E9\u05DC \u05D4\u05DE\u05E2\
  \u05E8\u05DB\u05EA. \u05E4\u05E2\u05D5\u05DC\u05D4 \u05D6\u05D5 \u05D9\u05E1\u05D5\
  \u05D3\u05D9\u05EA \u05DC\u05DE\u05E9\u05D9\u05DE\u05D5\u05EA \u05DB\u05D2\u05D5\
  \u05DF \u05E8\u05D9\u05E9\u05D5\u05DD \u05DC\u05D5\u05D2\u05D9\u05DD, \u05E7\u05D9\
  \u05D6\u05D5\u05D6 \u05D6\u05DE\u05E0\u05D9\u05DD \u05D0\u05D5 \u05E7\u05D1\u05DC\
  \u05EA \u05D4\u05D7\u05DC\u05D8\u05D5\u05EA \u05E2\u05DC \u05E1\u05DE\u05DA\u2026"
lastmod: 2024-02-19 22:04:58.972829
model: gpt-4-0125-preview
summary: "\u05D0\u05D9\u05D7\u05D6\u05D5\u05E8 \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA\
  \ \u05D4\u05E0\u05D5\u05DB\u05D7\u05D9 \u05D1-PowerShell \u05E7\u05E9\u05D5\u05E8\
  \ \u05DC\u05E9\u05DC\u05D9\u05E4\u05EA \u05EA\u05D0\u05E8\u05D9\u05DA \u05D5\u05E9\
  \u05E2\u05D4 \u05E0\u05D5\u05DB\u05D7\u05D9\u05EA \u05E9\u05DC \u05D4\u05DE\u05E2\
  \u05E8\u05DB\u05EA. \u05E4\u05E2\u05D5\u05DC\u05D4 \u05D6\u05D5 \u05D9\u05E1\u05D5\
  \u05D3\u05D9\u05EA \u05DC\u05DE\u05E9\u05D9\u05DE\u05D5\u05EA \u05DB\u05D2\u05D5\
  \u05DF \u05E8\u05D9\u05E9\u05D5\u05DD \u05DC\u05D5\u05D2\u05D9\u05DD, \u05E7\u05D9\
  \u05D6\u05D5\u05D6 \u05D6\u05DE\u05E0\u05D9\u05DD \u05D0\u05D5 \u05E7\u05D1\u05DC\
  \u05EA \u05D4\u05D7\u05DC\u05D8\u05D5\u05EA \u05E2\u05DC \u05E1\u05DE\u05DA\u2026"
title: "\u05E7\u05D1\u05DC\u05EA \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05D4\u05E0\
  \u05D5\u05DB\u05D7\u05D9"
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
