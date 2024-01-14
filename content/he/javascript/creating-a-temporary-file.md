---
title:    "Javascript: יצירת קובץ זמני"
keywords: ["Javascript"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/he/javascript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## למה?

יצירת קובץ זמני בג'אווהסקריפט יכול להיות מאוד שימושי כאשר מתכנתים רוצים ליצור קבצים באופן דינמי בתוך התוכנית שלהם. קבצים זמניים יכולים לשמש למטרות מגוונות כמו שמירת נתונים זמנים, ניתוח נתונים ועוד.

## איך לעשות זאת?

כדי ליצור קובץ זמני בג'אווהסקריפט, ישנם כמה שלבים פשוטים שעליכם לעקוב אחריהם.

```Javascript
// גזירת קובץ זמני עם שם ונתיב
var tempFile = new File(["sample text"], "tempFile.txt", {type: "text/plain"});

// שמירת הקובץ כקובץ זמני בתוך התוכנית שלנו
var fileURL = URL.createObjectURL(tempFile);

// קריאת הקובץ והדפסת התוכן שלו
fetch(fileURL)
.then(response => response.text())
.then(text => console.log(text));
```

הפלט שלתוכן הקובץ הזמני ייראה כך:

> "sample text"

כמו כן, ניתן גם לשנות את סוג הקובץ זמני בעזרת שימוש בפרמטרים נוספים בתוך הפינקציה הראשונה.

## Deep Dive

קבצים זמניים בג'אווהסקריפט הם בעצם קבצי מקור שנוצרים בכתובת אתר שלך כאשר אתה מפעיל פונקציה כמו `createObjectURL()`. דבר זה מגביל את התוכן שמוכנס לקובץ לתושבת בקבצי מקור שלא יכולים להופיע בשפה תגית אינטרנט רגילה. עם זאת, קובצים זמניים הם מאוד שימושיים לבדיקות, ניתוח נתונים ומטרות תצוגה גלויות.

## ראה גם

- [קבצים זמניים בג'אווהסקריפט](https://developer.mozilla.org/he/docs/Web/API/URL/createObjectURL)
- [צורת פיקוד JSON עם פסקת Markdown - דגם במהירות](https://jsonformatter.org/markdown-table-formatter)
-