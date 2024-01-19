---
title:                "קריאה של ארגומנטים משורת הפקודה"
html_title:           "C#: קריאה של ארגומנטים משורת הפקודה"
simple_title:         "קריאה של ארגומנטים משורת הפקודה"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## מה זה ולמה? 

קריאה של ארגומנטים מהשורה הפקודה היא דרך לקבל נתונים מהמשתמש שיכולים לשנות את התנהגות התוכנית. מתכנתים משתמשים בכך כדי לתת גמישות למשתמשים ולאפשר שינויים דינאמיים בהתנהגות התוכנית.

## איך:

הנה דוגמא של קיבולת ארגומנטים מהשורה הפקודה ב- TypeScript:

```TypeScript
// args.ts
function main() {
    const args = process.argv.slice(2);
    args.forEach((value, index) => {
    console.log(`Arg ${index}: ${value}`);
    });
}

main();
```
הרצת תוכנית ה- TypeScript אם נעביר לה את הארגומנטים "hi" ו- "hello":

```
$ tsc args.ts && node args.js hi hello
```
תוצאה:

```
Arg 0: hi
Arg 1: hello
```
## הצצה לעומק:

לקרוא ארגומנטים מהשורה הפקודה הוא פרקטיקה נפוצה בתיכנות. היא משמשת בראש ובראשונה בשפות תכנות המתחילות מהשורה הפקודה, אך בשנים האחרונות היא מתפשטת לתוכניות מחשב רגילות. חלופה אחרת היא שימוש בקלט משתמש דרך ממשק המשתמש.


ל- TypeScript, `process.argv` הוא מערך שמכיל את הארגומנטים של שורת הפקודה. המספרים שנתונים ל- `slice(2)` הם כדי להתעלם מהשני הראשונים במערך, שם התוכנית והחלק המוגדר תמיד.

## ראה גם:

פרטים נוספים על איך להשתמש ב- `process.argv`:
- [MDN Web Docs - process.argv](https://nodejs.org/docs/latest/api/process.html#process_process_argv)
- [Stack Overflow - How to pass command line arguments to a Node.js program?](https://stackoverflow.com/questions/4351521/how-do-i-pass-command-line-arguments-to-a-node-js-program)