---
title:                "כתיבה לפלט השגיאה הסטנדרטי"
html_title:           "Arduino: כתיבה לפלט השגיאה הסטנדרטי"
simple_title:         "כתיבה לפלט השגיאה הסטנדרטי"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## מה ולמה?
כתיבה ל-Standard Error (stderr) היא דרך להדפיס הודעות שגיאה או דיאגנוסטיקה. תכניתנים עושים זאת כדי להבחין בין פלט רגיל לבין הודעות על תקלות ובעיות בזמן ריצה.

## איך לעשות:
```TypeScript
// דוגמא לכתיבה ל-standard error ב-TypeScript
console.error('זוהי הודעת שגיאה.');

// ייצוא פונקציה שמדפיסה שגיאה מותאמת אישית ל-standard error
function reportError(errorMessage: string): void {
    console.error(`שגיאה: ${errorMessage}`);
}

reportError('משהו השתבש!');  // זה ידפיס "שגיאה: משהו השתבש!"
```
פלט הדוגמה:
```
זוהי הודעת שגיאה.
שגיאה: משהו השתבש!
```

## עיון מעמיק:
בהיסטוריה, הבחנה בין פלט רגיל ל-stderr הייתה חשובה בסביבות Unix, כדי לאפשר הפניית שגיאות לקבצים או למסופים אחרים. ב-TypeScript, אנחנו משתמשים ב-`console.error` כברירת מחדל, אבל יש גם אלטרנטיבות כמו ייצוא לקבצי לוג דרך ספריות חיצוניות. הכתיבה ל-stderr מתבצעת דרך ה-API של סביבת הריצה, למשל Node.js.

## ראה גם:
- [Console.error() – MDN web docs](https://developer.mozilla.org/en-US/docs/Web/API/Console/error)
- [Node.js process.stderr – Node.js Documentation](https://nodejs.org/api/process.html#processstderr)

טיפים ושימושים נוספים ב-stderr ב-TypeScript ניתן למצוא במסמכים ובפורומים המוקדשים לפיתוח תוכנה.