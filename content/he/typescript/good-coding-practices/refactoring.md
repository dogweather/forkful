---
date: 2024-01-26 03:37:18.613473-07:00
description: "\u05E9\u05D9\u05E4\u05D5\u05E5 \u05E7\u05D5\u05D3 \u05D4\u05D5\u05D0\
  \ \u05D4\u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05DC \u05DE\u05D1\u05E0\u05D4 \u05DE\
  \u05D7\u05D3\u05E9 \u05E9\u05DC \u05E7\u05D5\u05D3 \u05DE\u05D7\u05E9\u05D1 \u05E7\
  \u05D9\u05D9\u05DD \u05DE\u05D1\u05DC\u05D9 \u05DC\u05E9\u05E0\u05D5\u05EA \u05D0\
  \u05EA \u05D4\u05EA\u05E0\u05D4\u05D2\u05D5\u05EA\u05D5 \u05D4\u05D7\u05D9\u05E6\
  \u05D5\u05E0\u05D9\u05EA. \u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\
  \u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D4\u05E4\u05D5\u05DA\
  \ \u05D0\u05EA \u05D4\u05E7\u05D5\u05D3 \u05DC\u05E0\u05E7\u05D9 \u05D9\u05D5\u05EA\
  \u05E8, \u05E0\u05D9\u05EA\u05DF \u05DC\u05EA\u05D7\u05D6\u05D5\u05E7\u05D4 \u05D9\
  \u05D5\u05EA\u05E8,\u2026"
lastmod: '2024-03-13T22:44:38.931396-06:00'
model: gpt-4-0125-preview
summary: "\u05E9\u05D9\u05E4\u05D5\u05E5 \u05E7\u05D5\u05D3 \u05D4\u05D5\u05D0 \u05D4\
  \u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05DC \u05DE\u05D1\u05E0\u05D4 \u05DE\u05D7\
  \u05D3\u05E9 \u05E9\u05DC \u05E7\u05D5\u05D3 \u05DE\u05D7\u05E9\u05D1 \u05E7\u05D9\
  \u05D9\u05DD \u05DE\u05D1\u05DC\u05D9 \u05DC\u05E9\u05E0\u05D5\u05EA \u05D0\u05EA\
  \ \u05D4\u05EA\u05E0\u05D4\u05D2\u05D5\u05EA\u05D5 \u05D4\u05D7\u05D9\u05E6\u05D5\
  \u05E0\u05D9\u05EA."
title: "\u05E8\u05E4\u05E7\u05D8\u05D5\u05E8\u05D9\u05E0\u05D2"
weight: 19
---

## איך לעשות:
שקלו פונקציה ב-TypeScript שהייתה בימים טובים יותר - היא בלגן קצת, ויכולה להיטיב משהו חביבה ותשומת לב:

```typescript
function userInfo(data: any): string {
    return "User Info: " + data.name + ", " + data.age + ", " + data.email + ";" ;
}
```
לאחר שיפוץ, זה יכול להיראות כך:

```typescript
interface User {
    name: string;
    age: number;
    email: string;
}

function formatUserInfo(user: User): string {
    return `User Info: ${user.name}, ${user.age}, ${user.email};`;
}
```

הדוגמא השנייה היא יציבה יותר, מנצלת את מערכת הטיפוסים של TypeScript עם `interface` בכדי למנוע שגיאות זמן ריצה פוטנציאליות ולשפר את הקריאות.

## צלילה עמוקה
שיפוץ אינו מושג מודרני; הוא התפתח יחד עם התכנות, והפך להיות יותר מסודר עם פרסום הספר של מרטין פאולר "Refactoring: Improving the Design of Existing Code" ב-1999. זה קריטי בסביבת פיתוח Agile, מקל על שינויי קוד מותאמים. חלופות לשיפוץ ידני כוללות כלים אוטומטיים כמו TSLint או שרת השפה של TypeScript שיכולים להציע או אף לבצע משימות שיפוץ מסוימות בשבילך. פרטי היישום בדרך כלל כוללים זיהוי של "ריחות קוד", כמו קוד כפול, מתודות ארוכות, או מחלקות גדולות, ויישום תבניות לתיקון—כמו חילוץ מתודות, העברה למחלקות מתאימות יותר, או שימוש במבנים פשוטים יותר. תבניות אלו הן מפתח להבנת האיך והלמה של שיפוץ.

## ראה גם
- [הספר "Refactoring: Improving the Design of Existing Code" מאת מרטין פאולר](https://martinfowler.com/books/refactoring.html)
- [TSLint לניתוח קוד סטטי](https://palantir.github.io/tslint/)
- [הבנת ריחות קוד](https://refactoring.guru/refactoring/smells)
