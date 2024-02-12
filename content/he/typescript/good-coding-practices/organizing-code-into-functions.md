---
title:                "ארגון קוד לתוך פונקציות"
aliases: - /he/typescript/organizing-code-into-functions.md
date:                  2024-01-26T01:16:58.084185-07:00
model:                 gpt-4-0125-preview
simple_title:         "ארגון קוד לתוך פונקציות"

tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/organizing-code-into-functions.md"
---

{{< edit_this_page >}}

## מה ולמה?
ארגון קוד לתוך פונקציות משמעותו חלוקת הקוד לבלוקים מודולריים וניתנים לשימוש חוזר. אנו עושים זאת על מנת לשמור על העקרון של DRY (Don't Repeat Yourself – אל תחזור על עצמך), ובכך להפוך את הקוד לנקי יותר, קל יותר לקריאה ולאיתור באגים.

## איך לעשות זאת:
דמיינו שאתם יוצרים מחשבון בסיסי. במקום לכתוב את הלוגיקה של החיבור בכל מקום בו אתם זקוקים לה, צרו פונקציה של `add`:

```TypeScript
function add(x: number, y: number): number {
  return x + y;
}

console.log(add(5, 7)); // פלט לדוגמה: 12
```

עכשיו, בואו נאמר שאנו צריכים פונקציה להכפלה:

```TypeScript
function multiply(x: number, y: number): number {
  return x * y;
}

console.log(multiply(3, 4)); // פלט לדוגמה: 12
```
שמתם לב איך אנחנו מתמקדים במשימה אחת לכל פונקציה? זהו ליבה של ארגון קוד.

## צלילה עמוקה
בהיסטוריה, ככל ששפות התכנות התפתחו, פונקציות הפכו לחשובות במבנה הקוד, על בסיס השאיבה מפונקציות מתמטיות. הן יסוד בתכנות הליך ונמשכות בפרדיגמות של תכנות מונחה עצמים ותכנות פונקציונלי.

אלטרנטיבות? יכולים לבחור שלא להשתמש בפונקציות, אבל זו כרטיס בודד לעיר הספגטי. או שתוכלו ללכת על OOP (תכנות מונחה עצמים) ולארוז פונקציונליות לתוך שיטות – שהן בעצם פונקציות השייכות לעצמים.

מבחינת היישום, TypeScript מתעקש על טיפוסים. הגדרת טיפוסי קלט ופלט לפונקציות אינה רק מנהג טוב; היא חובה לקוד TypeScript נקי. ובנוסף, עם TypeScript, אתם מקבלים תכונות מועילות כמו overloads, generics, ופרמטרים אופציונליים לשדרוג הפונקציות שלכם.

## ראו גם
בדקו את המקורות האלה לשיפור הידע שלכם בנושא פונקציות:

- [מדריך TypeScript – פונקציות](https://www.typescriptlang.org/docs/handbook/2/functions.html): התנ"ך שלכם לפונקציות ב-TypeScript.
- [קוד נקי JavaScript](https://github.com/ryanmcdermott/clean-code-javascript#functions): החלו את עקרונות קוד נקי על הפונקציות שלכם ב-JavaScript.
- [אתה לא יודע JS – טווח וסגירות](https://github.com/getify/You-Dont-Know-JS): הבינו איך פונקציות עובדות עם טווח וסגירות ב-JavaScript.
