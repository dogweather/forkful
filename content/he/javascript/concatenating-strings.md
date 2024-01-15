---
title:                "מחבר תווים"
html_title:           "Javascript: מחבר תווים"
simple_title:         "מחבר תווים"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

## למה:

אף אחד לא אוהב מחרוזות מגושפות. אבל, בעולם התכנות זה חלק לא ניתן למנוע. לכן, בשפת ג'אווהסקריפט, יש כמה סיבות מדוע נדרש לשרשר מחרוזות. הראשון הוא שמחרוזת אחת לא מספיקה כדי ליצור מערכת טקסט מלאה מעוצבת. צריך להוסיף פרטים שונים כמו שם, טלפון, כתובת לכל אדם וליהנות מכל המשתמע מהדיווחים בתקציב וחשבוניות.

## איך לעשות זאת:

הנה כמה דוגמאות של שרשור מחרוזות בשפת ג'אווהסקריפט:
```Javascript
let firstName = "John";
let lastName = "Doe";
let fullName = firstName + " " + lastName;
console.log(fullName);
```

הפלט של קוד זה יהיה "John Doe".

עוד דוגמה:
```Javascript
let sentenceStart = "My favorite color is ";
let color = "blue";
let sentence = sentenceStart + color + ".";
console.log(sentence);
```

הפלט של קוד זה יהיה "My favorite color is blue.".

## חקירת מעמקים:

שיטת השרשור היא אחת הערכות יסודיות שבשפת ג'אווהסקריפט והיא משמשת להתחברות של מחרוזת למחרוזת אחרת. בג'אווהסקריפט, מחרוזות מסומנות בסוגריים כפולים וניתן לכתוב על ידי שימוש באופרטור "+" כדי לשרשר אותן יחד. יכול להיות מצבים כאשר יש צורך לשרשר מספרים ומחרוזות ביחד. כדי לעשות זאת, יש להמיר את המספרים למחרוזות באמצעות הפעולה toString().

## לראות גם:

- פרק על מחרוזות בעמוד הדוקומנטציה הרשמי של ג'אווהסקריפט: https://developer.mozilla.org/he/docs/Web/JavaScript/Reference/Global_Objects/String
- טקסט מקוון זמין עם דוגמאות נוספות: https://www.w3schools.com/js/js_string_concat.asp
- כתבה מעניינת על טכניקות שרשור מחרוזות