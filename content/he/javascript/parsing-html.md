---
title:                "ניתוח HTML"
html_title:           "Javascript: ניתוח HTML"
simple_title:         "ניתוח HTML"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/parsing-html.md"
---

{{< edit_this_page >}}

## מה ומדוע? 
פענוח HTML הוא תהליך שבו מתוכנתים ממירים את קוד ה-HTML של דף אינטרנט לפורמט נתונים המובן למכשירים טכנולוגיים. הפענוח הוא חיוני לתפעול של דפי אינטרנט ולהצגתם כמיני מידע ותמונות. תוכנתי חושבים על הקוד הלא תקין של אתרים כאשר הם מנסים לפענט HTML ולתקן אותם, מה שמביא לטובת התפעול ולשיפור הגרפיקה.

## איך לעשות?
קודם כל, נא להבין שפענוח HTML הוא תהליך כתיבת קוד בה שכל הפרטים היטב להיות בעלי אופי מיוצמים טכנולוגים. כאן יעשו הרבה מחשבות המשתמשים לפענוח-קבלה-תציידי HTML מותאמים לכל פיתוח נתון. זה כל כך פשוט לכתוב קוד HTML יש לפענח אותו, כי יותר ויותר אנשים מתנסים את כל קודי ה-HTML שהורדו באתר.

קוד קבוע הצגה של דף באמצעות הפענוח.ן

```Javascript
const html = '<p>Hello World!</p>';
const parser = new DOMParser();
const parsedHTML = parser.parseFromString(html, 'text/html');
console.log(parsedHTML.body.innerHTML);
// Output: Hello World!
```

## לחקור 
ראשית כל, הפענוח HTML נמצא עם הטכנולוגיות המתאימות לזמן הנכון. זה נמצא עם כלי הפענוח הכלים שהיו משתמשים בהם כשהם רושמים באתר חשוב שכתרת הכלים עדיין בוגולציה. פענוף המידע הוא חשוב לפמוח כמו גם לשבוד. לפענוח כונן.

## ראה גם
- [HTML תיקוני] https://developer.mozilla.org/en-US/docs/Web/API/DOMParser
- [תיקוני HTML כלים און העולמות] http://www.html5reporter.com
- [תיקוני HTML נרגיל] https://www.w3.org/TR/html-parsing/