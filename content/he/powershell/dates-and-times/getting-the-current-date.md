---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:10:57.399869-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: PowerShell \u05DE\
  \u05E1\u05E4\u05E7\u05EA cmdlets \u05E4\u05E9\u05D5\u05D8\u05D5\u05EA \u05DC\u05E7\
  \u05D1\u05DC\u05EA \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05D5\u05D4\u05E9\u05E2\
  \u05D4. \u05D4-cmdlet `Get-Date` \u05D4\u05D5\u05D0 \u05D4\u05DB\u05DC\u05D9 \u05D4\
  \u05E2\u05D9\u05E7\u05E8\u05D9 \u05DC\u05DE\u05D8\u05E8\u05D4 \u05D6\u05D5. \u05D4\
  \u05D5\u05D0 \u05D9\u05DB\u05D5\u05DC \u05DC\u05D4\u05D7\u05D6\u05D9\u05E8 \u05D0\
  \u05EA \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05D5\u05D4\u05E9\u05E2\u05D4 \u05D4\
  \u05DE\u05DC\u05D0\u05D9\u05DD, \u05D0\u05D5\u05EA\u05DD\u2026"
lastmod: '2024-03-13T22:44:39.716180-06:00'
model: gpt-4-0125-preview
summary: "PowerShell \u05DE\u05E1\u05E4\u05E7\u05EA cmdlets \u05E4\u05E9\u05D5\u05D8\
  \u05D5\u05EA \u05DC\u05E7\u05D1\u05DC\u05EA \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA\
  \ \u05D5\u05D4\u05E9\u05E2\u05D4."
title: "\u05E7\u05D1\u05DC\u05EA \u05D4\u05EA\u05D0\u05E8\u05D9\u05DA \u05D4\u05E0\
  \u05D5\u05DB\u05D7\u05D9"
weight: 29
---

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
