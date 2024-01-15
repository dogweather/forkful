---
title:                "השתמשות בביטויים רגילים"
html_title:           "TypeScript: השתמשות בביטויים רגילים"
simple_title:         "השתמשות בביטויים רגילים"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## למה

כתיבת ביטויים רגולריים היא כלי חזק וגמיש לטיפול בטקסטים בתכנות TypeScript. היא מאפשרת לנו למצוא ולהחליף חלקי טקסט, לבדק תקינות של כתבי ת.ז ועוד.

## איך לעשות זאת

```TypeScript
// בניית ביטוי רגולרי כדי למצוא כל כתובת דוא"ל
const emailRegex = /^\w+([-+.]\w+)*@\w+([-.]\w+)*\.\w+([-.]\w+)*$/;

// ביטוי רגולרי שמחזיר את שם המשתמש באתר אינטרנט
const usernameRegex = /^((http|https):\/\/)?(www\.)?[a-z0-9]+([\-\.]{1}[a-z0-9]+)*\.[a-z]+(\/.*)?$/i;

// ביטוי רגולרי לחיפוש מספקים פרטיים מתוך כתובת דוא"ל
const providerRegex = /@(.+)/;

// בדיקת אם המחרוזת מתאימה לביטוי רגולרי
console.log(emailRegex.test('test@example.com')); // יחזיר: true
console.log(usernameRegex.test('https://www.example.com')); // יחזיר: true
console.log(providerRegex.exec('test@example.com')[1]); // יחזיר: example.com
```

## מכה עמוקה

ביטויים רגולריים נמצאים בשפות תכנות רבות והם משמשים בגישה חכמה ויעילה יותר לעיבוד טקסטים. כדי להשתמש בהם בצורה טובה, יש ללמוד חוקי התחביר והפונקציות הכתיבה של ביטויים רגולריים.

## ראו גם

- [מדריך לביטויים רגולריים עבור מתכנתים של TypeScript](https://www.regular-expressions.info/javascript.html)
- [כלי לבדיקת תקינות ביטויים רגולריים עבור TypeScript](https://regex101.com/)