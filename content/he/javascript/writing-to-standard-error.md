---
title:                "כתיבה לפלט שגיאה סטנדרטי"
html_title:           "Javascript: כתיבה לפלט שגיאה סטנדרטי"
simple_title:         "כתיבה לפלט שגיאה סטנדרטי"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/writing-to-standard-error.md"
---

{{< edit_this_page >}}

ניתן לחשוב על כתיבה למספר שגיאה כאופציה שימושית במקרים בהם אנו רוצים לקבל מידע מורחב על טעויות ושגיאות של התוכנית שלנו. לדוגמה, במקרים של תיקון באגים או ניטור ריצה של קובץ קומפיוטר גדול.

## מדוע

מכיוון שכתיבה למספר שגיאה מאפשרת לנו לפנות ישירות לקובץ השגיאות שתוכננו לצורך ניתוח מפורט וטיפול בשגיאות. זה יכול להיות מאוד שימושי במקרים שברצוננו לבחון את התוכנית שלנו בכדי למזער את פעמי ההתקלות ולהגביר את הביצועים שלה.

## איך לעשות זאת

נתחיל עם דוגמא פשוטה באמצעות פונקציה המדפיסה את ההודעה שלנו לתקן שגיאה לקובץ שגיאות:

```javascript
console.error("אופס, משהו השתבש!");
```

פלט:

```
אופס, משהו השתבש!
```

כעת, אם נרצה להוסיף פרטים נוספים להודעת השגיאה שלנו, ניתן להשתמש במתודה `console.error` כדי להדפיס את ההודעה ואת כתובת השגיאה שנמצאת בתכונת `stack` של האובייקט של השגיאה:

```javascript
try {
  // קוד שגיאה פועל כאן
  throw new Error("שגיאה מיוחדת!");
} catch (err) {
  // הדפסת השגיאה וכתובת השגיאה:
  console.error("ארעה תקלה:");
  console.error(err.stack);
};
```

פלט:

```
ארעה תקלה:
Error: שגיאה מיוחדת!
    at Object.<anonymous> (repl:3:9)
    at Module._compile (internal/modules/cjs/loader.js:711:30)
    at evalScript (internal/bootstrap/node.js:564:27)
    at internal/bootstrap/node.js:606:5
    at eval (native)
    at run (internal/bootstrap/run_script.js:397:8)
    at startup (internal/bootstrap/node.js:157:9)
    at bootstrapNodeJSCore (internal/bootstrap/node.js:533:3)
```

## Deep Dive

בדוגמאות הקוד שראינו עד כה, היו משתמשים בפונק