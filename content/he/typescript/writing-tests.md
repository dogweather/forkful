---
title:                "כתיבת מבחנים"
html_title:           "TypeScript: כתיבת מבחנים"
simple_title:         "כתיבת מבחנים"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## מה זה ולמה?

כתיבת בדיקות היא תהליך שבו מתכנתים כותבים קוד נוסף כדי לבדוק את תקינות הקוד שלהם. מתכנתים עושים זאת כדי לוודא שהקוד שלהם עובד כצפוי ושלא קיימים בעיות בקוד שייתכן לגרום לתקלות ולבאגים בתוכנה.

## איך לעשות:

באמצעות TypeScript, ניתן לכתוב בדיקות עבור כל מחלקה או פונקציה בקוד. לדוגמה, ניתן להשתמש בהגדרת `describe` כדי לכתוב מחלקות של בדיקות. בנוסף, ניתן להשתמש בהגדרת `it` כדי לכתוב בדיקות ספציפיות.

```TypeScript
describe('Calculator', () => {
  it('should add two numbers correctly', () => {
    const calculator = new Calculator();
    const result = calculator.add(2, 2);
    expect(result).toEqual(4);
  });
});
```

כאשר הבדיקות נכתבות בצורה נכונה, אז תיתכן פלט מוצלח שכזה:

```
Calculator
  ✓ should add two numbers correctly
```

## עומק נכנס:

כתיבת בדיקות נמצאת כבר זמן רב בתחום התכנות והינה כלי חשוב במיוחד כאשר מדובר בפיתוח תוכנה מורכבת. ישנם כמה אלטרנטיבות כמו כן לכתיבת בדיקות כדי לוודא את תקינות הקוד. כמו כן, קיימים טכניקות נוספות כדי לשפר את מהירות הבדיקות כמו בתוכנה Jest.

## ראה גם:

למידע נוסף על אופן כתיבת בדיקות ב-TypeScript, ניתן לקרוא את המדריך המפורט באתר הרשמי של TypeScript: https://www.typescriptlang.org/docs/handbook/testing.html  כמו כן, ניתן לעיין בתיעוד הרשמי של Jest על אופן שימוש בטכניקות נוספות כדי לשפר את הבדיקות: https://jestjs.io/docs/en/getting-started