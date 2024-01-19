---
title:                "קריאת קובץ טקסט"
html_title:           "Go: קריאת קובץ טקסט"
simple_title:         "קריאת קובץ טקסט"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

# לקרוא קובץ טקסט ב־TypeScript: מדריך מהיר

## מה & למה?
קריאת קובץ טקסט היא תהליך שבו תוכנית מקלטת נתונים מקובץ ואז מפענחת ומשתמשת בנתונים אלו. תכנתים עושים את זה כדי לאכוף את הנתונים או לשמוע דינמיקה בזמן שהתוכנית רצה.

## איך לעשות:
הנה כיצד אתה יכול לקרוא קובץ טקסט ב־TypeScript, באמצעות מודול Node.js, `fs`.

```TypeScript
import * as fs from 'fs';

fs.readFile('/path/your-file.txt', 'utf8', (err, data) => {
  if (err) {
    console.error(err);
    return;
  }
  console.log(data);
});
```

בדוגמה זו, המחרוזת '/path/your-file.txt' היא הנתיב של הקובץ שאתה רוצה לקרוא.

## צלילה עמוקה
(1) בהקשר ההיסטורי, Node.js ומודול ה-`fs` שלו הם לא תלות חלק מTypeScript. TypeScript היא מעין שדרוג של JavaScript המסייע לתכנתים לכתוב קוד מסודר יותר.
(2) ישנם שיטות אלטרנטיביות רבות לקריאת קבצים ב־JavaScript וגם ב־TypeScript, כולל באמצעות AJAX.
(3) לגבי פרטי היישום, `readFile` הוא אסינכרוני, מה שאומר שהתוכנית שלך תמשיך לרוץ בזמן שהיא מחכה לחזרה המידע מהקובץ. אם תרצה לקרוא את הקובץ באופן סינכרוני, יתכן שתרצה לשקול להשתמש במתודה `readFileSync`.

## ראה גם:
- [תיעוד Node.js fs](https://nodejs.org/api/fs.html)
- [קריאת קבצים בJavaScript עם AJAX](https://www.w3schools.com/js/js_ajax_intro.asp)
- [מדריך לאיך להשתמש בTypeScript](https://www.typescriptlang.org/docs/handbook/intro.html)