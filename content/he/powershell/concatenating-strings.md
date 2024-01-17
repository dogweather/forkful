---
title:                "לחבר מחרוזות"
html_title:           "PowerShell: לחבר מחרוזות"
simple_title:         "לחבר מחרוזות"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/concatenating-strings.md"
---

{{< edit_this_page >}}

# מה זה ולמה?

טיפוס מחרוזות (concatenating strings) הוא פעולה שמאגדת שתי או יותר מחרוזות למחרוזת יחידה. זוהי פעולה נפוצה שבתכנות ומשמשת לצורך שילוב מידע מסוגים שונים אל מחרוזות אחת גדולה יותר.

# איך לעשות:

הנה כמה דוגמאות לכתיבת תכניות בפני עצמן עם דוגמאות לפלט נתונים:

```PowerShell
# דוגמא 1: כמווות שתי מחרוזות יחד
$string1 = "שלום"
$string2 = "עולם"
$string3 = $string1 + $string2

# פלט מצורף: שלוםעולם

# דוגמא 2: צריבת מחרוזת ומספר
$name = "נועה"
$age = 29
$string = "שמי הוא $name וגילי הוא $age."

# פלט מצורף: שמי הוא נועה וגילי הוא 29.
```

# טיול עמוק:

טיפוס מחרוזות הוא פעולה נפוצה בשפות תכנות רבות כולל PowerShell. השימוש בפעולה זו יכול להיות מוצדק כאשר מתארגן מידע מעורך ומפנים. ישנן חלופות לטיפוס מחרוזות, כמו זמן ריצה (runtime) של פעולות תשתיתיות של המערכת של העורך כגון פלט משני אלמנטים וידוע כסטרינגשפורמט (string.format). פלט המשני אלמנטים יכול לכלול קלט מצורף, טקסט מבורר ואתר, כאשר פעולות יותר מורכבות מוכנסות כמידע בשורת עורך של המחרוזת.

# ראה גם:

למידע נוסף על עשיית טיפוס מחרוזת ראה:

- [מסמכי ידע של Microsoft אודות טיפוס מחרוזות](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.core/about/about_concatenating_strings?view=powershell-7)
- [מידע נוסף על טיפוסי מחרוזות מבחינת תוכנית](https://www.powershellgallery.com//about/error?) 
- [דוגמאות נוספות על טיפוס מחרוזות בפורום של Microsoft](https://social.technet.microsoft.com/Forums),he-He/5a2f0bcc-a7f2-47c4-b706-3ffd9eff4e%3A0*- ?view=pt-BR).