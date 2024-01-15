---
title:                "כתיבת מבחנים"
html_title:           "Javascript: כתיבת מבחנים"
simple_title:         "כתיבת מבחנים"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/writing-tests.md"
---

{{< edit_this_page >}}

## למה

כתיבת מבחנים נחשבת לפעולה חשובה בתכנות ב-Javascript כי היא מאפשרת למתכנתים לבדוק את תקינות הקוד שלהם ולחסוך זמן ואנרגיה באיתור ותיקונם של באגים לאחר שנשוני התוכנה.

## כיצד לעשות זאת

עבור דוגמא, נדגים איך לכתוב בדיקות ב-Javascript בעזרת הספרייה Jest:

```Javascript
// ייבוא מודולים והגדרת פונקציות תמיכה
const functions = require('./functions');

// מבחן לבדיקת תקינות פונקציה ספציפית
test('פונקציית add מוסיפה נכונה', () => {
  expect(functions.add(1, 2)).toBe(3);
});

// מבחן לבדיקת תקינות פונקציה אחרת
test('פונקציית multiply מכפלה נכונה', () => {
  expect(functions.multiply(2, 2)).toBe(4);
});

// מבחן עבור פונקציה מסוימת שציפינו לקרוס
test('פונקציית divide אמורה לקרוס כשמחלקים ב- 0', () => {
  expect(() => functions.divide(2, 0)).toThrowError();
});

// מבחן לבדיקת תקינות עבור אריח שמיוחד למוח
test('getTile מחזיר ריק כאשר המיקום של אריח המוח הוא 0', () => {
  expect(functions.getTile(0)).toBe('');
});
```

תוצאות המבחנים יהיו:

```Javascript
 PASS  ./functions.test.js
  ✓ add פונקציית מכונסת (2 ms)
  ✓ פונקציית multiply מכפלה נכונה (1 ms)
  ✓ פונקציית divide אמורה לקרוס כשמחלקים ב -0
  ✓ getTile מחזיר ריק כאשר המיקום של אריח המוח הוא 0 (1 ms)

Test Suites: 1 passed, 1 total
Tests:       3 passed, 3 total
Snapshots:   0 total
Time:        0.69 s
Ran all test suites.
```

## לצלול עומק יותר

כתיבת מבחנים נותנת למתכנתים את היכולת לבדוק כוונה ופעילות של כל חלק בקוד ולהבטיח שהם עובדים כך כפי שצריך. היא גם מאפש