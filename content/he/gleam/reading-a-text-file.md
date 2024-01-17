---
title:                "קריאת קובץ טקסט"
html_title:           "Gleam: קריאת קובץ טקסט"
simple_title:         "קריאת קובץ טקסט"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

מה ולמה?

קריאת קובץ טקסט היא פעולה נפוצה בקודים של מתכנתים. כאשר אנחנו קוראים קובץ טקסט, אנחנו מקבלים את תוכן הקובץ המתוצרף בתוך תוכנית הקוד שלנו. מתכנתים עושים זאת כדי לטפל בנתונים מבחוץ, כגון קבצי טקסט או נתונים ממסדי נתונים חיצוניים.

כיצד?

כתיבת קוד לקריאת קובץ טקסט היא דבר פשוט וקל. כל מה שאנחנו צריכים לעשות הוא להשתמש בפונקציות המיוחדות של Gleam כדי לסרוק את הקובץ ולקרוא את תוכנו. הנה דוגמאות של קוד ותוצאה:

```Gleam
let file = File.open("example.txt")
let content = File.read_all(file)
```

הדפסת תוכן הקובץ:

```Gleam
let file = File.open("example.txt")
let content = File.read_all(file)
IO.println(content)
```

למעבר על כל שורה בקובץ והדפסתה:

```Gleam
let file = File.open("example.txt")
File.each_line(file, fn line ->
  IO.println(line)
end)
```

מעמיקים יותר...

קריאת קובץ טקסט היא נפוצה מאוד בתחום השפות התכנות וישנם כמה דרכים מרתיעות נוספות לטפל ולקרוא נתונים מקבצים. בעבר, קוד כזה נכתב בדרך כלל בשפת C באמצעות פונקציות כמו fopen, fgets ו-fclose. בשפת Gleam, ההתקנה של מודול חיצוני כמו gleam_stdlib קלה יותר ומספקת פונקציות משופרות כדי לעזור לקורא קבצים לטפל בקריאת קבצים כגון File.read_all ו-File.each_line.

ראה גם

- [פורום דיונים על כתיבת קובץ וקריאת קובץ בשפת Gleam](https://forum.gleam.run/t/reading-writing-files-in-gleam/27)
- [מסמך הדרכה רשמי על קריאת קבצים ב-Gleam](https://gleam.run/book/std/files.html)