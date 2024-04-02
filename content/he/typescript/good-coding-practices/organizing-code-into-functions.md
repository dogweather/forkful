---
date: 2024-01-26 01:16:58.084185-07:00
description: "\u05D0\u05E8\u05D2\u05D5\u05DF \u05E7\u05D5\u05D3 \u05DC\u05EA\u05D5\
  \u05DA \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05EA \u05DE\u05E9\u05DE\u05E2\
  \u05D5\u05EA\u05D5 \u05D7\u05DC\u05D5\u05E7\u05EA \u05D4\u05E7\u05D5\u05D3 \u05DC\
  \u05D1\u05DC\u05D5\u05E7\u05D9\u05DD \u05DE\u05D5\u05D3\u05D5\u05DC\u05E8\u05D9\u05D9\
  \u05DD \u05D5\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05DC\u05E9\u05D9\u05DE\u05D5\
  \u05E9 \u05D7\u05D5\u05D6\u05E8. \u05D0\u05E0\u05D5 \u05E2\u05D5\u05E9\u05D9\u05DD\
  \ \u05D6\u05D0\u05EA \u05E2\u05DC \u05DE\u05E0\u05EA \u05DC\u05E9\u05DE\u05D5\u05E8\
  \ \u05E2\u05DC \u05D4\u05E2\u05E7\u05E8\u05D5\u05DF \u05E9\u05DC DRY (Don't Repeat\
  \ Yourself \u2013 \u05D0\u05DC\u2026"
lastmod: '2024-03-13T22:44:38.926509-06:00'
model: gpt-4-0125-preview
summary: "\u05D0\u05E8\u05D2\u05D5\u05DF \u05E7\u05D5\u05D3 \u05DC\u05EA\u05D5\u05DA\
  \ \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05EA \u05DE\u05E9\u05DE\u05E2\u05D5\
  \u05EA\u05D5 \u05D7\u05DC\u05D5\u05E7\u05EA \u05D4\u05E7\u05D5\u05D3 \u05DC\u05D1\
  \u05DC\u05D5\u05E7\u05D9\u05DD \u05DE\u05D5\u05D3\u05D5\u05DC\u05E8\u05D9\u05D9\u05DD\
  \ \u05D5\u05E0\u05D9\u05EA\u05E0\u05D9\u05DD \u05DC\u05E9\u05D9\u05DE\u05D5\u05E9\
  \ \u05D7\u05D5\u05D6\u05E8. \u05D0\u05E0\u05D5 \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\
  \u05D0\u05EA \u05E2\u05DC \u05DE\u05E0\u05EA \u05DC\u05E9\u05DE\u05D5\u05E8 \u05E2\
  \u05DC \u05D4\u05E2\u05E7\u05E8\u05D5\u05DF \u05E9\u05DC DRY (Don't Repeat Yourself\
  \ \u2013 \u05D0\u05DC\u2026"
title: "\u05D0\u05E8\u05D2\u05D5\u05DF \u05E7\u05D5\u05D3 \u05DC\u05EA\u05D5\u05DA\
  \ \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05EA"
weight: 18
---

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
