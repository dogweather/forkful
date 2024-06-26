---
date: 2024-01-20 17:54:51.796405-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: (How to:) \u05E7\u05E8\
  \u05D9\u05D0\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\u05D8 \u05D9\u05E9\
  \u05E8 \u05DC\u05DE\u05E9\u05EA\u05E0\u05D4."
lastmod: '2024-04-05T21:53:40.819709-06:00'
model: gpt-4-1106-preview
summary: "(How to:) \u05E7\u05E8\u05D9\u05D0\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\
  \u05E7\u05E1\u05D8 \u05D9\u05E9\u05E8 \u05DC\u05DE\u05E9\u05EA\u05E0\u05D4."
title: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05E7\u05D5\u05D1\u05E5 \u05D8\u05E7\u05E1\
  \u05D8"
weight: 22
---

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
