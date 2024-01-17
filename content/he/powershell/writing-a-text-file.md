---
title:                "כתיבת קובץ טקסט"
html_title:           "PowerShell: כתיבת קובץ טקסט"
simple_title:         "כתיבת קובץ טקסט"
programming_language: "PowerShell"
category:             "PowerShell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/writing-a-text-file.md"
---

{{< edit_this_page >}}

# מה ולמה?

כתיבת קובץ טקסט היא פעולה נפוצה בעולם התכנות. היא מציינת הכנת קובץ שמכיל טקסט פשוט, כגון מידע טקסטואלי או ערכים נומריים. מתכנתים משתמשים בכתיבת קבצי טקסט כדי לאחסן מידע בצורה מסודרת ונגישה.

# איך לעשות?

הנה כמה דוגמאות לכתיבת קובץ טקסט בפווורשל. כדי להריץ קוד PowerShell, השתמש בפקודה ```powershell``` ואחריה הקוד הרלוונטי.

כדי ליצור קובץ טקסט חדש, ניתן להשתמש בפקודת ```New-Item``` עם הפרמטר -ItemType "file", ולספק את השם של הקובץ כארגומנט:

```
powershell
New-Item -ItemType "file" -Path "example.txt"
```

כדי להוסיף טקסט לקובץ קיים, ניתן להשתמש בפקודת ```Add-Content```, עם הפרמטר -Path לציין את הקובץ והטקסט להוסיף כארגומנט:

```
powershell
Add-Content -Path "example.txt" -Value "Hello, world!"
```

כדי לקרוא את תוכן הקובץ, ניתן להשתמש בפקודת ```Get-Content``` ולציין את הקובץ כארגומנט:

```
powershell
Get-Content "example.txt"
```

כעת תוכלו לראות בפלט ש-HellO, world! נוסף לקובץ.

# טיול עמוק

בעבר, לפני שהומצאו שפות תכנות מתקדמות כמו PowerShell, כתיבת קבצי טקסט הייתה מתבצעת באמצעות ספר נייד ועט. עם התפתחות הטכנולוגיות, לכתיבת קבצי טקסט יצאו לאור תוכנות שונות כגון Notepad ו-WordPad.

בנוסף לכתיבה ידנית, קיימות גם פקודות שונות בפווורשל המאפשרות לנו לנהל, לערוך ולתפעל על קבצי טקסט בצורה פעילה ומתקדמת.

אם אתם מעוניינים לקרוא עוד על כתיבת קבצי טקסט בפווורשל, אתם מוזמנים לבדוק את המקורות המצורפים בסוף המאמר.

# ראה גם

- https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/new-item
- https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/add-content
- https://docs.microsoft.com/en-us/powershell/module/microsoft.powershell.management/get-content