---
date: 2024-01-26 04:17:11.599466-07:00
description: "\u05D4\u05DE\u05E2\u05D8\u05E4\u05EA \u05D4\u05D0\u05D9\u05E0\u05D8\u05E8\
  \u05D0\u05E7\u05D8\u05D9\u05D1\u05D9\u05EA, \u05D0\u05D5 \u05DC\u05D5\u05DC\u05D0\
  \u05EA \u05E7\u05E8\u05D9\u05D0\u05D4-\u05D7\u05D9\u05E9\u05D5\u05D1-\u05D4\u05D3\
  \u05E4\u05E1\u05D4 (REPL), \u05DE\u05D0\u05E4\u05E9\u05E8\u05EA \u05DC\u05DA \u05DC\
  \u05D4\u05E7\u05DC\u05D9\u05D3 \u05E4\u05E7\u05D5\u05D3\u05D5\u05EA PowerShell \u05D5\
  \u05DC\u05E7\u05D1\u05DC \u05E4\u05D9\u05D3\u05D1\u05E7 \u05DE\u05D9\u05D9\u05D3\
  \u05D9. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\
  \u05D9\u05DD \u05D1\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05D1\u05D3\u05D5\u05E7 \u05E7\
  \u05D8\u05E2\u05D9 \u05E7\u05D5\u05D3\u2026"
lastmod: '2024-02-25T18:49:37.939526-07:00'
model: gpt-4-0125-preview
summary: "\u05D4\u05DE\u05E2\u05D8\u05E4\u05EA \u05D4\u05D0\u05D9\u05E0\u05D8\u05E8\
  \u05D0\u05E7\u05D8\u05D9\u05D1\u05D9\u05EA, \u05D0\u05D5 \u05DC\u05D5\u05DC\u05D0\
  \u05EA \u05E7\u05E8\u05D9\u05D0\u05D4-\u05D7\u05D9\u05E9\u05D5\u05D1-\u05D4\u05D3\
  \u05E4\u05E1\u05D4 (REPL), \u05DE\u05D0\u05E4\u05E9\u05E8\u05EA \u05DC\u05DA \u05DC\
  \u05D4\u05E7\u05DC\u05D9\u05D3 \u05E4\u05E7\u05D5\u05D3\u05D5\u05EA PowerShell \u05D5\
  \u05DC\u05E7\u05D1\u05DC \u05E4\u05D9\u05D3\u05D1\u05E7 \u05DE\u05D9\u05D9\u05D3\
  \u05D9. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\
  \u05D9\u05DD \u05D1\u05D4 \u05DB\u05D3\u05D9 \u05DC\u05D1\u05D3\u05D5\u05E7 \u05E7\
  \u05D8\u05E2\u05D9 \u05E7\u05D5\u05D3\u2026"
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05DE\u05E2\u05D8\u05E4\u05EA \u05D0\
  \u05D9\u05E0\u05D8\u05E8\u05D0\u05E7\u05D8\u05D9\u05D1\u05D9\u05EA (REPL)"
---

{{< edit_this_page >}}

## מה ולמה?
המעטפת האינטראקטיבית, או לולאת קריאה-חישוב-הדפסה (REPL), מאפשרת לך להקליד פקודות PowerShell ולקבל פידבק מיידי. מתכנתים משתמשים בה כדי לבדוק קטעי קוד במהירות, לאבחן באגים, או ללמוד פקודות חדשות בלי לכתוב סקריפט מלא.

## איך ל:
הפעל את PowerShell ואתה ב-REPL. נסה את Cmdlet `Get-Date`:

```PowerShell
PS > Get-Date
```

אתה אמור לראות את התאריך והשעה הנוכחיים:

```PowerShell
רביעי, 31 במרץ 2023 12:34:56 PM
```

עכשיו, שרשר פקודות. בוא נמיין תהליכים לפי שימוש בזיכרון:

```PowerShell
PS > Get-Process | Sort-Object WS -Descending | Select-Object -First 5
```

זה מציג את חמשת התהליכים הראשיים לפי גודל הסט העובד (שימוש בזיכרון).

## צלילה עמוקה
REPL של PowerShell נובע ממעטפת Unix ומעטפות של שפות דינמיות אחרות כמו של Python. זו סביבה אינטראקטיבית לביצוע פקודות למשתמש יחיד. בניגוד לשפה מומצאת שבה אתה כותב יישומים שלמים ואז מהדר, סביבת REPL מאפשרת לך לכתוב ולהריץ קוד תיק לתיק. PowerShell תומך גם בביצוע סקריפטים למשימות גדולות יותר.

חלופות ל-Windows כוללות את הפקודה Prompt או REPLs ספציפיים לשפה כמו IPython. בעולם של Unix/Linux, מעטפות כמו bash או zsh מתפקדות בתפקיד דומה.

היישום של PowerShell משתמש באפליקציית מארח כדי להריץ את המעטפת. אם כי PowerShell.exe ב-Windows הוא הנפוץ ביותר, ישנם אחרים כמו Integrated Scripting Environment (ISE) או הטרמינל המשולב של Visual Studio Code שגם יכולים לשמש כמארח.

## ראה גם
- [אודות PowerShell](https://docs.microsoft.com/en-us/powershell/scripting/overview)
- [StackOverflow: PowerShell](https://stackoverflow.com/questions/tagged/powershell)
