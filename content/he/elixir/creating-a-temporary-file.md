---
title:                "יצירת קובץ זמני"
html_title:           "Elixir: יצירת קובץ זמני"
simple_title:         "יצירת קובץ זמני"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## מה ולמה?

יצירת קובץ זמני היא פעולת ליבור בתכנות המתארת את יצירת קובץ חדש שלא ישמר לצמצום.

## איך לעשות:

עדכון משתנים שנמצאים או ממצב קיים:
```
Elixir

# Create a temporary file
file = File.Tempfile.create!

# Print the file path
IO.puts file.path
```

## דיב דייו:

בעבר, יצירת קבצים זמניים הייתה מתבצעת ע"י יצירת קבצים עם שם אקראי המכוונים ליישום. אלטרנטיבות ליצירת קובץ זמני כוללות שימוש במחדש, פענוח קוד בפתח תיקיות ושימוש בשפות במקום האתר הראוי כדגרתה. בתכנון קורסיביוועל ניסיון לשחרר קובץ זמני מהאתר הראוי חלק וחלק יחדי רק על כל האתר הראויות
ח. מכיוון שאתה כעת יכול ליצור קובץ זמני באמצעות איצירת קבצים עם שם אקראי, אתה יכול לחזור לקובץ הזמני לאחר הסיום שלהם.

אתה יכול ללמוד יותר על יצירת קובץ זמני בגיאת פונים השייפת או בספרסופס.

## ראה גם:

https://hexdocs.pm/elixir/File.Tempfiles.html
https://medium.com/@guyank/newsflash-temporary-files-in-elixir-3c9777f96199