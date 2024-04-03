---
changelog:
- 2024-01-30, gpt-4-0125-preview, translated from English
date: 2024-01-30 19:13:56.773392-07:00
description: "\u05DE\u05E2\u05E8\u05DB\u05D9 \u05E9\u05D9\u05D5\u05DA, \u05D0\u05D5\
  \ \u05D0\u05D5\u05D1\u05D9\u05D9\u05E7\u05D8\u05D9\u05DD \u05D1-TypeScript, \u05DE\
  \u05D0\u05E4\u05E9\u05E8\u05D9\u05DD \u05DC\u05DB\u05DD \u05DC\u05D4\u05E9\u05EA\
  \u05DE\u05E9 \u05D1\u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA (\u05D0\u05D5 \u05DE\
  \u05E4\u05EA\u05D7\u05D5\u05EA) \u05DB\u05D3\u05D9 \u05DC\u05D2\u05E9\u05EA \u05DC\
  \u05D6\u05D5\u05D2\u05D5\u05EA \u05E2\u05E8\u05DA. \u05DE\u05EA\u05DB\u05E0\u05EA\
  \u05D9\u05DD \u05DE\u05E9\u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\u05D4\u05DD \u05DC\
  \u05EA\u05D1\u05E0\u05D9\u05D5\u05EA \u05D2\u05D9\u05E9\u05D4 \u05DC\u05E0\u05EA\
  \u05D5\u05E0\u05D9\u05DD \u05D9\u05D5\u05EA\u05E8 \u05D3\u05D9\u05E0\u05DE\u05D9\
  \u05D5\u05EA\u2026"
lastmod: '2024-03-13T22:44:38.905634-06:00'
model: gpt-4-0125-preview
summary: "\u05DE\u05E2\u05E8\u05DB\u05D9 \u05E9\u05D9\u05D5\u05DA, \u05D0\u05D5 \u05D0\
  \u05D5\u05D1\u05D9\u05D9\u05E7\u05D8\u05D9\u05DD \u05D1-TypeScript, \u05DE\u05D0\
  \u05E4\u05E9\u05E8\u05D9\u05DD \u05DC\u05DB\u05DD \u05DC\u05D4\u05E9\u05EA\u05DE\
  \u05E9 \u05D1\u05DE\u05D7\u05E8\u05D5\u05D6\u05D5\u05EA (\u05D0\u05D5 \u05DE\u05E4\
  \u05EA\u05D7\u05D5\u05EA) \u05DB\u05D3\u05D9 \u05DC\u05D2\u05E9\u05EA \u05DC\u05D6\
  \u05D5\u05D2\u05D5\u05EA \u05E2\u05E8\u05DA."
title: "\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05DE\u05E2\u05E8\u05DB\u05D9\u05DD\
  \ \u05D0\u05E1\u05D5\u05E6\u05D9\u05D0\u05D8\u05D9\u05D1\u05D9\u05D9\u05DD"
weight: 15
---

## איך לעשות:
יצירה ושימוש במערכי שיוך ב-TypeScript היא תהליך ישיר. הנה סקירה בסיסית:

```TypeScript
// הצהרה על מערך שיוך
let user: { [key: string]: string } = {};

// הוספת נתונים
user["name"] = "Jane Doe";
user["email"] = "jane@example.com";

console.log(user);
```

פלט:

```TypeScript
{ name: 'Jane Doe', email: 'jane@example.com' }
```

לדרוך על זוגות מפתח-ערך גם כן קל:

```TypeScript
for (let key in user) {
    console.log(key + ": " + user[key]);
}
```

פלט:

```TypeScript
name: Jane Doe
email: jane@example.com
```

ואם אתם מתמודדים עם סוגי נתונים מעורבים, מערכת הטיפוסים של TypeScript מתגלה כשימושית:

```TypeScript
let mixedTypes: { [key: string]: string | number } = {};
mixedTypes["name"] = "John Doe";
mixedTypes["age"] = 30;

console.log(mixedTypes);
```

פלט:

```TypeScript
{ name: 'John Doe', age: 30 }
```

## צלילה עמוקה
ב-TypeScript, מה שאנו מתייחסים אליו כמערכי שיוך הם למעשה אובייקטים. מבחינה היסטורית, בשפות כמו PHP, מערכי שיוך הם סוג בסיסי, אך JavaScript (ובהרחבה, TypeScript) משתמשת באובייקטים למטרה זו. גישה זו היא גם חוזק וגם הגבלה. אובייקטים מספקים מבנה דינמי ביותר לשיוך מחרוזות לערכים, אך הם אינם מיועדים לשימוש כמערכים במובן המסורתי. לדוגמה, לא ניתן להשתמש בשיטות מערך כמו `push` או `pop` ישירות על אובייקטים אלו.

למקרים בהם יש צורך באוספים מסודרים של זוגות מפתח-ערך עם פעולות דמויות מערך, TypeScript (וה-JavaScript המודרני) מציעה את האובייקט `Map`:

```TypeScript
let userMap = new Map<string, string>();
userMap.set("name", "Jane Doe");
userMap.set("email", "jane@example.com");

userMap.forEach((value, key) => {
    console.log(key + ": " + value);
});
```

תוך כדי שמערכת הטיפוסים של TypeScript ותכונות ES6 כמו `Map` מספקות חלופות עוצמתיות, הבנה של כיצד להשתמש באובייקטים כמערכי שיוך שימושית לתרחישים שבהם אובייקטים ליטרליים הם יעילים יותר או כאשר עובדים עם מבני נתונים של JSON. הכול עניין של בחירה בכלי הנכון למשימה.
