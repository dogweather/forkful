---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:29:36.826729-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: PowerShell \u05DE\
  \u05E1\u05E4\u05E7\u05EA cmdlets \u05E4\u05E9\u05D5\u05D8\u05D9\u05DD \u05DC\u05E0\
  \u05D9\u05D4\u05D5\u05DC \u05E7\u05D1\u05E6\u05D9\u05DD. \u05D4-cmdlet `Out-File`\
  \ \u05D5\u05D0\u05D5\u05E4\u05E8\u05D8\u05D5\u05E8\u05D9 \u05D4\u05D4\u05E4\u05E0\
  \u05D9\u05D9\u05D4 \u05DE\u05E9\u05DE\u05E9\u05D9\u05DD \u05D1\u05E2\u05D9\u05E7\
  \u05E8 \u05DC\u05DE\u05D8\u05E8\u05D4 \u05D6\u05D5. \u05DC\u05D4\u05DC\u05DF \u05D3\
  \u05D5\u05D2\u05DE\u05D0\u05D5\u05EA \u05D4\u05DE\u05DE\u05D7\u05D9\u05E9\u05D5\u05EA\
  \ \u05D0\u05D9\u05DA \u05DC\u05DB\u05EA\u05D5\u05D1 \u05D8\u05E7\u05E1\u05D8\u2026"
lastmod: '2024-03-13T22:44:39.728770-06:00'
model: gpt-4-0125-preview
summary: "PowerShell \u05DE\u05E1\u05E4\u05E7\u05EA cmdlets \u05E4\u05E9\u05D5\u05D8\
  \u05D9\u05DD \u05DC\u05E0\u05D9\u05D4\u05D5\u05DC \u05E7\u05D1\u05E6\u05D9\u05DD\
  ."
title: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8"
weight: 24
---

## איך לעשות:
PowerShell מספקת cmdlets פשוטים לניהול קבצים. ה-cmdlet `Out-File` ואופרטורי ההפנייה משמשים בעיקר למטרה זו. להלן דוגמאות הממחישות איך לכתוב טקסט לקבצים בתרחישים שונים:

**יצירת קובץ טקסט בסיסי:**

ליצירת קובץ טקסט וכתיבת מחרוזת פשוטה אליו, ניתן להשתמש:

```powershell
"Hello, World!" | Out-File -FilePath .\example.txt
```

או שקול עם אופרטור הכוונה:

```powershell
"Hello, World!" > .\example.txt
```

**הוספת טקסט לקובץ קיים:**

אם ברצונך להוסיף טקסט לסוף קובץ קיים מבלי לדרוס אותו:

```powershell
"Another line." | Out-File -FilePath .\example.txt -Append
```

או באמצעות אופרטור הכוונה להוספה:

```powershell
"Another line." >> .\example.txt
```

**כתיבת מספר שורות:**

לכתיבת מספר שורות, ניתן להשתמש במערך של מחרוזות:

```powershell
$lines = "Line 1", "Line 2", "Line 3"
$lines | Out-File -FilePath .\multilines.txt
```

**ציון קידוד מסוים:**

לצורך ציון קידוד טקסט מסוים, השתמש בפרמטר `-Encoding`:

```powershell
"Text with UTF8 Encoding" | Out-File -FilePath .\utfexample.txt -Encoding UTF8
```

**שימוש בספריות צד שלישי:**

למרות ש-cmdlets המובנים של PowerShell מספיקים לפעולות קובץ בסיסיות, משימות מורכבות יותר עשויות להרוויח ממודולים של צד שלישי כמו `PowershellGet` או כלים כמו `SED` ו`AWK` שהותאמו ל-Windows. עם זאת, לצורך כתיבה פשוטה של קובץ טקסט, אלה עשויים להיות מוגזמים ובדרך כלל אינם נדרשים:

```powershell
# בהנחה שתרחיש מורכב יותר מצדיק שימוש בספרייה חיצונית
# Install-Module -Name SomeComplexLibrary
# Import-Module -Name SomeComplexLibrary
# פעולות מורכבות יותר כאן
```

_הערה: תמיד שקול האם המורכבות של הוספת תלות בצד שלישי מוצדקת לצרכיך._

**תוצאה לדוגמה:**

לאחר ביצוע הפקודה ליצירת קובץ בסיסי, בדיקת תוכן `example.txt` מציגה:

```plaintext
Hello, World!
```

עבור הוספת טקסט ואז בדיקת `example.txt`:

```plaintext
Hello, World!
שורה נוספת.
```
