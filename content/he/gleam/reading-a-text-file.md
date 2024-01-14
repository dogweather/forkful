---
title:    "Gleam: קריאת קובץ טקסט"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/gleam/reading-a-text-file.md"
---

{{< edit_this_page >}}

## למהלמה לקרוא קובץ טקסט:
לקריאת קובץ טקסט יש הרבה אפשרויות ויתרונות. היא מאפשרת לנו לקרוא ולעבוד עם מידע מורכב בפורמט נוח ונגיש, וגם לקבל מידע מקורי ועדכני.

## כיצד לעשות זאת:
כדי לקרוא קובץ טקסט בשפת Gleam, ניתן להשתמש בפונקציה `File.read` ולספק לה את הכתובת של הקובץ. לדוגמה, נוכל לכתוב:

```Gleam
let text =
  File.read("myfile.txt")
```

ניתן להדפיס את המידע שנקרא מהקובץ באמצעות פונקציית ההדפסה `io.println`. נוכל להוסיף גם תנאים לבדיקת תקינות הקלט ועיבוד המידע המוחזר כמו בדוגמה הבאה:

```Gleam
let result =
  File.read("myfile.txt")
  
case result {
  Ok(text) -> io.println(text)
  Error(err) -> io.println("Error reading file: " ++ err)
}
```

הפונקציה `File.read` יכולה להחזיר ערך מסוג `Result` שהמידע שהוא מכיל יכול להיות מסוגים שונים, כגון מחרוזות, מספרים או רשימות. בהתאם, נוכל לעבוד עם המידע המוחזר בהתאם לצורך.

## צלילה עמוקה:
כדי לקרוא קובץ טקסט עם קונטקסט יותר מתקדם ומורכב, ניתן להשתמש בספריית התוספת `ripplerose/gleam-file`, המציעה תכונות נוספות כמו קריאת קבצים בפורמטים שונים, כגון CSV או JSON, ויכולות לעבוד עם קבצים מגוונים כמו תמונות או סרטונים.

## ראו גם:
- תיעוד רשמי של הפונקציה `File.read` באתר Gleam: [https://gleam.run/modules/file#read](https://gleam.run/modules/file#read)
- ספריית התוספת `ripplerose/gleam-file` ב-GitHub: [https://github.com/ripplerose/gleam-file](https://github