---
title:                "מציאת אורך של מחרוזת"
html_title:           "PowerShell: מציאת אורך של מחרוזת"
simple_title:         "מציאת אורך של מחרוזת"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## מה ולמה?

למצוא את האורך של מחרוזת הוא פעולה נפוצה בעולם התכנות. זהו פשוט מספר התווים במחרוזת, והחשיבה היא שהפעולה תעזור לנו לעבוד עם המידע הנתון בצורה יעילה יותר.

## איך לעשות זאת:

בכדי למצוא את האורך של מחרוזת עם PowerShell, נוכל להשתמש בפקודת "Len". נכניס את המחרוזת שנרצה לבדוק את אורכה, ונקבל כתוצאה את מספר התווים במחרוזת.

```PowerShell
$str = "שלום"
Len $str
```

פלט:
```PowerShell
4
```

ניתן גם למצוא את האורך של מחרוזת דרך המתודה ".Length", כך:

```PowerShell
$str = "שלום"
$str.Length
```

פלט:
```PowerShell
4
```

בנוסף, ניתן גם להשתמש במחברת PowerShell כדי לבדוק את אורך המחרוזת עם הפעולה הבאה:

```PowerShell
$str = "שלום"
$str | Measure-Object -Property Length
```

פלט:
```PowerShell
Count    : 1
Average  : 
Sum      : 
Maximum  : 4
Minimum  : 
Property : Length
```

## קפיצה מצומצמת:

אם נרצה למצוא את האורך של מחרוזת בשפות אחרות, נוכל להשתמש בפעולה דומה או בממשק גרפי המאפשר לנו להעביר את המחרוזת כפרמטר ולקבל כתוצאה את האורך שלה.

כמו כן, קיימות גם כלי ניתוח בונים בשפת PowerShell שמאפשרים לנו לחלץ מידע נוסף מהמחרוזת, כמו כמה פעמים מופיע מילה מסוימת במחרוזת או האם היא מתארת מספר או תאריך תקין.

## ייתכן שיהיו פתרונות טובים יותר:

אם אנו עובדים עם מחרוזות גדולות, ייתכן שהפעולה "Len" לא תהיה הפתרון הטוב ביותר. אנו ניתן להשתמש במתודת ".Length" במקום, שינוח את המידע כדי לאפשר עיבוד מהיר יותר.

## ראה גם:

מידע נוסף על אופניים להתמודדות עם מחרוזות בשפת PowerShell ניתן למצוא במקורות הבאים:

- [Docs.Microsoft.com](https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/get-content?view=powershell-7.1)
- [Docs.PowerShell.org](https://docs.vmware.com/srivate-cloud/2020/vcenter-server-command-line-interface/index.html)
- [GitHub.com](https://github.com/PowerShell/PowerShell/tree/master/docs)

עכשיו שאתה יודע כיצד למצוא את האורך של מחרוזת בשפת PowerShell, תוכל להשתמש בכך לפריימינג של קוד ולבצע פעולות בעזרת מידע נתון ויעיל. שימוש מצליח!