---
title:                "כתיבה לפלט השגיאה הסטנדרטי"
date:                  2024-01-19
html_title:           "Arduino: כתיבה לפלט השגיאה הסטנדרטי"
simple_title:         "כתיבה לפלט השגיאה הסטנדרטי"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבה ל-stderr היא דרך להדפיס הודעות שגיאה או אינפורמציה דיאגנוסטית לסטרים 'שגיאה סטנדרטית'. תכניתנים עושים זאת כדי לנתק את הפלט הרגיל מההודעות האלו.

## איך לעשות:
```javascript
// כיצד להדפיס ל stderr
console.error('זו שגיאה!');

// איך להפנות את הפלט של stderr לקובץ:
process.stderr.write('זו גם שגיאה!\n');
```
הפלט המתקבל:
```
זו שגיאה!
זו גם שגיאה!
```

## הצלילה לעומק
בימים של מערכות UNIX, הפרדת פלטים ל-stdout ו-stderr אפשרה למשתמשים להפריד בין מידע רגיל לבין הודעות שגיאה. חלופות לכתיבה ל-stderr כוללות יומני אירועים או רישום לקובץ לוג. ההפרדה בין stdout ל-stderr בג'אווהסקריפט באה לידי ביטוי באמצעות `console.log` לפלט רגיל ו`console.error` או `process.stderr.write` לפלט שגיאות.

## ראו גם
- [מסמך Node.js לעבודה עם stdout ו stderr](https://nodejs.org/api/process.html#process_process_stdout)
- [מדריך MDN לאובייקט ה-console](https://developer.mozilla.org/en-US/docs/Web/API/Console)
