---
title:                "רפקטורינג"
date:                  2024-01-26T03:37:18.613473-07:00
model:                 gpt-4-0125-preview
simple_title:         "רפקטורינג"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/refactoring.md"
---

{{< edit_this_page >}}

## מה ולמה?
שיפוץ קוד הוא התהליך של מבנה מחדש של קוד מחשב קיים מבלי לשנות את התנהגותו החיצונית. תכנתים עושים זאת כדי להפוך את הקוד לנקי יותר, ניתן לתחזוקה יותר, ולהקטין את המורכבות, מה שהופך אותו לקל יותר להבנה עבור מי שצולל אליו לראשונה.

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