---
title:                "קריאת קובץ טקסט"
aliases: - /he/powershell/reading-a-text-file.md
date:                  2024-01-20T17:54:51.796405-07:00
model:                 gpt-4-1106-preview
simple_title:         "קריאת קובץ טקסט"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/powershell/reading-a-text-file.md"
---

{{< edit_this_page >}}

## מה ולמה? (What & Why?)
קריאת קובץ טקסט ב-PowerShell זה פשוט לקחת תוכן מתוך קובץ ולהשתמש בו. מתכנתים עושים את זה כי לעיתים צריך לעבד נתונים, לקבל הגדרות או לשמור יומני רישום.

## איך לעשות: (How to:)
קריאת קובץ טקסט ישר למשתנה:
```PowerShell
$content = Get-Content -Path "C:\example.txt"
Write-Output $content
```
פלט לדוגמא:
```
שורה ראשונה בטקסט
שורה שנייה בטקסט
...
```

קריאה של קובץ לפי שורות:
```PowerShell
$lines = Get-Content -Path "C:\example.txt" -ReadCount 0
foreach ($line in $lines) {
  Write-Output $line
}
```
פלט לדוגמא:
```
שורה ראשונה בטקסט
שורה שנייה בטקסט
...
```

## עומק הצלילה: (Deep Dive)
פקודת `Get-Content` ב-PowerShell היא הדרך הסטנדרטית לקרוא קבצי טקסט. היסטורית, הפעולה הזו באה לקחת את מקומה של פקודות DOS כמו `type`. גישות אלטרנטיביות תואם לכלול את השימוש ב-[System.IO](System.IO) ב-.NET כדי לקבל אותן תוצאות.

קריאת קובץ עם [System.IO](System.IO):
```PowerShell
$reader = [System.IO.File]::OpenText("C:\example.txt")
try {
  while ($null -ne ($line = $reader.ReadLine())) {
    Write-Output $line
  }
}
finally {
  $reader.Close()
}
```

## ראה גם: (See Also)
- [Get-Content documentation](https://docs.microsoft.com/en-us/powershell/module/Microsoft.PowerShell.Management/Get-Content)
- [Understanding Streams, Redirection, and Write-Host in PowerShell](https://devblogs.microsoft.com/scripting/understanding-streams-redirection-and-write-host-in-powershell/)
