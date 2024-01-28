---
title:                "שימוש במעטפת אינטראקטיבית (REPL)"
date:                  2024-01-26T04:17:11.599466-07:00
model:                 gpt-4-0125-preview
simple_title:         "שימוש במעטפת אינטראקטיבית (REPL)"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/using-an-interactive-shell-repl.md"
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
