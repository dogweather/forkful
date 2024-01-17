---
title:                "מנתחים מחרוזת"
html_title:           "PowerShell: מנתחים מחרוזת"
simple_title:         "מנתחים מחרוזת"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/interpolating-a-string.md"
---

{{< edit_this_page >}}

# מה זה ולמה?

פריסת מחרוזת היא הפעולה של הכנסת ערכים נוספים, כגון משתנים או תוצאות מחשבון, בתוך מחרוזת. זה מאפשר לנו ליצור מחרוזת דינמית ולהציג ערכים מגוונים בתוך המחרוזת ללא צורך לחלץ אותם מחוץ לה. תכנתנים עושים את זה כדי לשפר זמינות וקריאות של קוד.

# איך לעשות?

כדי לבצע פריסת מחרוזת ב-PowerShell, יש להשתמש בסימן ה-% יחד עם המחרוזת והערכים הרצויים בין תוך סוגריים מסולסלים.למטה יש כמה דוגמאות של קוד ותוצאות:

```PowerShell
$fruit = "apple"
Write-Host "I like %fruit%s!"  # Output: I like apples!
```

```PowerShell
$number = 5
Write-Host "The number %number% is larger than 3." # Output: The number 5 is larger than 3.
```

# טיפול מעמיק

פריסת מחרוזת היא טכניקה נפוצה שמשמשת להצגת ערכים בתוך מחרוזת בתכנות. היא החלה כבר בשנות ה-60 של המאה ה-20 ומימים אלה היא נפוצה בשפות בתוכנות מטמון כגון Python ו-JavaScript. בפייתון, הטכניקה אופיינית בשימוש בתו ה-% כדי לפרס ערכים בתוך מחרוזת. בפועל, במחרוזת שמפריסה ערכים בונה על אופרטור ה-%, נקראת מחרוזת פורמטום.

קיימים גם טכניקות אחרות להצגת ערכים במחרוזת, כגון שמות כלשהם של שפות תכנות אחרות, למשל "מחרוזות תבנית" בשפת C ו-"תבניות ממזער" בשפת Go. עם זאת, פריסת מחרוזת היא עדיין טכניקה נפוצה ומועילה בתכנות.

# ראה גם

כדי למד עוד על פריסת מחרוזת בשפת PowerShell, ניתן להציץ במדריכים הבאים:

- [מדריך לפריסת מחרוזת ב-PowerShell](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_special_characters?view=powershell-7.1)
- [מדריך לפריסת מחרוזת ב-Python](https://docs.python.org/3/tutorial/inputoutput.html)
- [מדריך לפריסת מחרוזת ב-JavaScript](https://www.w3schools.com/jsref/jsref_obj_string.asp)