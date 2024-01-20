---
title:                "הדפסת פלט ניפוי שגיאות"
html_title:           "Arduino: הדפסת פלט ניפוי שגיאות"
simple_title:         "הדפסת פלט ניפוי שגיאות"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## מה & למה?

הדפסת האוטפוט לניפוי שגיאות היא כלים שמאפשר למתכנת לראות באיזו תהליך או פונקציה הוא נמצא בזמן שירות. הצורך בכך מתברר כאשר מתכנתים מנסים לזהות בעיות או שגיאות שתכניתם יכולה ליצור.

## איך לעשות:

אנו מתחילים עם הגדרת `console.log` הפשוטה ביותר ב- TypeScript:

```TypeScript
console.log("Hello, World!");
```

אם אתה מריץ את הקוד הזה, הפלט שלך יהיה:

```
Hello, World!
```

אפשר להדפיס יותר ממידע אחד:

```TypeScript
let world = "World";
console.log("Hello,", world);
```

פלט:

```
Hello, World
```
## צלילה עמוקה:

במהלך השנים, השיטה `console.log` גידלה להיות לא רק יעילה אלא גם מגוונת ביותר. שיטות בנפה מאפשרות למתכנתים לאפשר מעקבים, להוסיף מידע ולדמיין סטטוסים מסוימים.

אך עם זאת, יש גם חסרונות. השימוש הרחב ב- `console.log` יכול להוביל לקוד מעורבל. זו אחת הסיבות לקיומם של כלים מבוססים על מודולים כמו ספריית הניפוי "debug".

עבריית המידע מהשירות אל הקונסולה נעשית ישירות. ב-Node.js, הדפסה מנוקדת של תיקול המידע, מאפשרת את ההתמצאות בצורה נוחה ומהירה.

## ראה גם:

קרא ב- [MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/API/Console/log) על `console.log`. 

בדוק את [Node.js debug](https://www.npmjs.com/package/debug) לגישה מתקדמת ומתועשת יותר לניפוי.

בדוק את [JavaScript.info](https://javascript.info/debugging-chrome) למדריך איך להשתמש בכלים ניפוי מובנים בכרום.