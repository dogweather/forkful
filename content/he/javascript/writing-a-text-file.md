---
title:                "Javascript: כתיבת קובץ טקסט"
simple_title:         "כתיבת קובץ טקסט"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## למה
למה ישנו לכם להתעסק בכתיבת קובץ טקסט בקוד ג'אווהסקריפט? כי הכתיבה של קבצי טקסט היא חלק בלתי נפרד מתוכניות הקומפיוטר כיום. היא משמשת במגוון רחב של מטרות כגון שמירת מידע, אחסון נתונים והדפסת פלט.

## איך לעשות זאת
הנה כמה דוגמאות של קוד ג'אווהסקריפט כדי להדגים איך לכתוב קובץ טקסט ומה יהיה הפלט:

```Javascript
// כתיבת טקסט לקובץ באמצעות פקודת כתיבת טקסט
var fs = require('fs');

// כתיבת טקסט לקובץ בשם "text.txt"
fs.writeFile("text.txt", "זהו קבצי הטקסט הראשון שלי", function(err) {

    // בדיקה שאין שגיאות
    if(err) {
        return console.log(err);
    }

    // הדפסת הודעה אם הקובץ נכתב בהצלחה
    console.log("קובץ הטקסט נכתב כראוי!");
});

// קבלת תוכן של קובץ טקסט קיים באמצעות פקודת קריאת קובץ טקסט
var fs = require('fs');

// קריאת קובץ טקסט בשם "text.txt"
fs.readFile('text.txt', 'utf8', function (err, data) {

// בדיקה שאין שגיאות
if (err) {
    return console.log(err);
}

// הדפסת התוכן הקיים בקובץ
console.log(data);
});

```

```
פלט:

זהו קובץ טקסט הראשון שלי
```

## חקירה מעמיקה
לכתוב קבצי טקסט יכול לכלול גם תהליך נוסף שנקרא "הדפסת טקסט מערך". בתהליך זה, אנחנו משתמשים בלולאות עבור התאמת טקסט לפורמט מסוים או לצורך הדפסת אינדקסים. לדוגמה:

```Javascript
// הצהרה על מערך טקסט
var array = ["זה", "הוא", "מע