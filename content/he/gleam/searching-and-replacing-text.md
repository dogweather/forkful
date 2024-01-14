---
title:                "Gleam: חיפוש והחלפת טקסט"
programming_language: "Gleam"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/gleam/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## למה
למה לעסוק בחיפוש והחלפת טקסט יכול להיות כלי חשוב כאשר מתעסקים בתכנות. זה נותן אפשרות להפחית טעויות ולהפחית את זמן העבודה על קוד.

## איך לעשות
תחילה, עליכם להתחיל עם טעינת הקובץ שבו תרצו לחפש ולהחליף טקסט. לדוגמה, אם רוצים להחליף את כל המילים "אהבה" ל "געגוע" בקובץ שנמצא בנתיב "/myfolder/myfile.txt", אז נוכל לכתוב:

```Gleam
let file = File.read("/myfolder/myfile.txt")

let newFile = String.replace_all(file, "אהבה", "געגוע")

File.write("/myfolder/myfile.txt", newFile)
```

בקטע קוד זה, אנו טוענים את הקובץ, מבצעים החלפת טקסט ושומרים את הקובץ המעודכן בנתיב המקורי.

## עיון מעמיק
כאשר משתמשים בפונקציות חיפוש והחלפה כמו "find" ו- "replace_all", חשוב לזכור שהן פועלות רק על מחרוזת מסוימת והופכות אותה למחרוזת נוספת עם התיקונים/החלפות שביצענו. לכן, אם נרצה לבצע שינויים בקובץ או מספר קבצים, נצטרך להשתמש בלולאות כדי לעבור על כל הקבצים ולבצע את התיקונים המתאימים בכל אחד מהם.

## ראו גם
- [טפסים ומילונים בשפת גליאם](https://example.com/formulae-dictionaries-gleam)
- [מדריך לתכנות עם גליאמעם](https://example.com/guide-gleam)