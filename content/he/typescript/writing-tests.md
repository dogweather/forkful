---
title:                "TypeScript: כתיבת נבחנים"
simple_title:         "כתיבת נבחנים"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## למה

אנשים כתובים בדיקות כדי לוודא שהקוד שלהם עובד כצפוי ואין בו באגים. הבדיקות מאפשרות לקוד לתפקד מהר ולהיות יציב, כך שניתן להתמקד במטרות העיסקיות ולא בתיקוני באגים מתמיד.

## כיצד לכתוב בדיקות ב-TypeScript

בדיקות ב-TypeScript נכתבות באמצעות ספריית הבדיקות Jest. לפני שנתחיל, נצטרך להתקין את ספריית Jest באמצעות פקודת ההתקנה הבאה:

```TypeScript
npm install --save-dev jest
```

לאחר מכן, ניצור קובץ חדש לבדיקות, כמו למשל "calculator.spec.ts". בקובץ נכתוב את הקוד הבא:

```TypeScript
import { sum } from "./calculator"

test("adds 1 + 2 to equal 3", () => {
  expect(sum(1, 2)).toBe(3);
});
```

בקוד הנ"ל, אנו מייבאים את פונקציית החיבור מתוך הקובץ המקורי שלנו, "calculator.ts". לאחר מכן, אנו עוברים על הפונקציה עם הערך המצופה ומשתמשים בפונקציית "expect" כדי לוודא שהתוצאה של הפונקציה היא תואמת לערך המצופה.

כדי להריץ את הבדיקה, נצטרך להוסיף את הפקודה הבאה לקובץ ה-package.json שלנו:

```TypeScript
"test": "jest"
```

לאחר מכן, נוכל להריץ את הבדיקה באמצעות הפקודה "npm run test". אם כל הבדיקות עברו בהצלחה, תקבלו את הפלט הבא:

```TypeScript
PASS ./calculator.spec.ts
✓ adds 1 + 2 to equal 3 (4ms)
```

## צלילה עמוקה

בנוסף לבדיקות יחידות, ניתן לכתוב גם בדיקות עם נוסחאות לכיסוי קוד (code coverage), שיעזור לנו לוודא שחלקי הקוד שלנו מכוסים על ידי הבדיקות שלנו. ניתן גם ליצור מוקדם (mocks) שיאפשרו לנו לבדוק ח