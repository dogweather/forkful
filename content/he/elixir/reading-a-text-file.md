---
title:                "קריאת קובץ טקסט"
html_title:           "Go: קריאת קובץ טקסט"
simple_title:         "קריאת קובץ טקסט"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/elixir/reading-a-text-file.md"
---

{{< edit_this_page >}}

## ?מה ולמה 
קריאה של קובץ טקסט היא הפעולה שבה מריצים קוד פשוט כדי לחלות נתונים שנשמרו בקובץ טקסט. מתכנתים מבצעים את הפעולה הזו כדי לקרוא, לערוך או לנתח את המידע שהתקבל מהקובץ.

## לך איך:
קדימה, נראה לך שמה זה אומר בשפת Elixir:

```Elixir
{:ok, data} = File.read("path_to_your_file.txt")
IO.puts(data)
```

פשוט, לא? היה לך רק להחליף את "path_to_your_file.txt" עם הנתיב לקובץ טקסט שאתה רוצה לקרוא, והקוד יקרא את התוכן ויוציא אותו.

## צלילה עמוקה:
מעניין לדעת שבקובץ Elixir מציג קובץ באופן שקוף, מה שמאפשר לנו לעבוד באופן שקוף עם קובץ טקסט או קובץ בינארי. חלופות אחרות יכולות להכליל קריאה של קובץ בשפות תכנות אחרות כמו Python או Java, אך Elixir היא שפה פונקציונלית שמציעה מרחק מטען רחב של כלים שנותנים את אפשרות למתכנת לקרוא נתונים מקובץ באופן בטוח ויעיל.

## ראה גם:
1. https://hexdocs.pm/elixir/File.html - תיעוד אופציה 'File' ב-Elixir.
2. https://elixir-lang.org/getting-started/io-and-the-file-system.html - 'Elixir': IO ומערכת הקבצים.