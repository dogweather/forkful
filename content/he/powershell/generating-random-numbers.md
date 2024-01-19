---
title:                "יצירת מספרים אקראיים"
html_title:           "PowerShell: יצירת מספרים אקראיים"
simple_title:         "יצירת מספרים אקראיים"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/generating-random-numbers.md"
---

{{< edit_this_page >}}

## מה ולמה?

יצירת מספרים אקראיים היא תהליך שבו משתמשים בתוכניות תכנות כדי ליצור מספרים שאינם תדירים או קבועים כדי לבצע פעולות שונות. תכניות תכנות כמו PowerShell מאפשרות למפתחים ליצור מספרים אקראיים עם קלות ובמהירות, ולהשתמש בהם במגוון של צורות.

## כיצד לעשות זאת:

```PowerShell
# גישת "אבטחה אקראית" למספרים אקראיים בפווימ"ו
Get-Random [-Minimum <מספר מינימלי>] [-Maximum <מספר מקסימלי>] [-InputObject <ערכים>] [-Count <מספר מספרים לייצר>] [-IncludeExclude <העלאה או הדחה של ערכים>] [-SetSeed <זרע מספרי>] [-PassThru] [-?]

# להגדיר נתיב מקומי או רשת כארגומנט מובנה
[System.Random]::New().Next()
```

## מחקר מעמוק:

טכניקות ליצירת מספרים אקראיים נמצאות מאז העלאתם של מחשבים ותכניות לניתוח מספרי דגים ידניים עבור מתרחשות חמצוניות. מיון פוטנציאליות שלאינס (מורגים, עמ. 1936, בסיסני נצח) הוא גם דגים-היד מספרי לשיאים ניתנים לחדר ולהייצר איות התמיד כשעותהיצפדל שעותיחדל בעובור הנתונים הקיצוניים בהתחברות לתקענה לתפוקת פערי במקביל מתניים חמצוניים וממש נקודת מתרחשות.

## ראה גם:

למד שיטות נוספות ליצור מספרים אקראיים עם PowerShell:

- [Microsoftכתיבת לוח]
- [Generating Random Passwords with PowerShell]

כמו כן, ניתן למצוא מידע נוסף על נושאים דומים בהמשך.