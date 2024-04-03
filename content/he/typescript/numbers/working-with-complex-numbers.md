---
date: 2024-01-26 04:46:55.285267-07:00
description: "\u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05DE\u05E8\u05D5\u05DB\u05D1\u05D9\
  \u05DD, \u05D4\u05DE\u05D5\u05E8\u05DB\u05D1\u05D9\u05DD \u05DE\u05D7\u05DC\u05E7\
  \ \u05DE\u05DE\u05E9\u05D9 \u05D5\u05D7\u05DC\u05E7 \u05DE\u05D3\u05D5\u05DE\u05D4\
  \ (\u05E9\u05E0\u05DB\u05EA\u05D1 \u05D1\u05D3\u05E8\u05DA \u05DB\u05DC\u05DC \u05DB\
  -a + bi), \u05DE\u05D0\u05E4\u05E9\u05E8\u05D9\u05DD \u05D7\u05D9\u05E9\u05D5\u05D1\
  \u05D9\u05DD \u05E9\u05D4\u05D9\u05D5 \u05D1\u05DC\u05EA\u05D9-\u05D0\u05E4\u05E9\
  \u05E8\u05D9\u05D9\u05DD \u05D0\u05D5 \u05D1\u05DC\u05EA\u05D9 \u05DE\u05E2\u05E9\
  \u05D9\u05D9\u05DD \u05E2\u05DD \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05DE\u05DE\
  \u05E9\u05D9\u05D9\u05DD \u05D1\u05DC\u05D1\u05D3. \u05DE\u05EA\u05DB\u05E0\u05EA\
  \u05D9\u05DD\u2026"
lastmod: '2024-03-13T22:44:38.907362-06:00'
model: gpt-4-0125-preview
summary: "\u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05DE\u05E8\u05D5\u05DB\u05D1\u05D9\
  \u05DD, \u05D4\u05DE\u05D5\u05E8\u05DB\u05D1\u05D9\u05DD \u05DE\u05D7\u05DC\u05E7\
  \ \u05DE\u05DE\u05E9\u05D9 \u05D5\u05D7\u05DC\u05E7 \u05DE\u05D3\u05D5\u05DE\u05D4\
  \ (\u05E9\u05E0\u05DB\u05EA\u05D1 \u05D1\u05D3\u05E8\u05DA \u05DB\u05DC\u05DC \u05DB\
  -a + bi), \u05DE\u05D0\u05E4\u05E9\u05E8\u05D9\u05DD \u05D7\u05D9\u05E9\u05D5\u05D1\
  \u05D9\u05DD \u05E9\u05D4\u05D9\u05D5 \u05D1\u05DC\u05EA\u05D9-\u05D0\u05E4\u05E9\
  \u05E8\u05D9\u05D9\u05DD \u05D0\u05D5 \u05D1\u05DC\u05EA\u05D9 \u05DE\u05E2\u05E9\
  \u05D9\u05D9\u05DD \u05E2\u05DD \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05DE\u05DE\
  \u05E9\u05D9\u05D9\u05DD \u05D1\u05DC\u05D1\u05D3."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD \u05DE\u05E1\u05E4\u05E8\u05D9\
  \u05DD \u05DE\u05E8\u05D5\u05DB\u05D1\u05D9\u05DD"
weight: 14
---

## מה ולמה?
מספרים מרוכבים, המורכבים מחלק ממשי וחלק מדומה (שנכתב בדרך כלל כ-a + bi), מאפשרים חישובים שהיו בלתי-אפשריים או בלתי מעשיים עם מספרים ממשיים בלבד. מתכנתים משתמשים בהם בתחומים כמו עיבוד אותות, חישוב קוונטי, ומתמטיקה שימושית, שבהם הצגת מספרים בשני ממדים היא הכרחית.

## איך לעשות:
טיפול במספרים מרוכבים ב-TypeScript דורש מחלקה מיוחדת. בואו ניצור אחת ונעבוד דרכה על חיבור וכפל.

```TypeScript
class Complex {
    constructor(public re: number, public im: number) {}

    add(other: Complex): Complex {
        return new Complex(this.re + other.re, this.im + other.im);
    }

    multiply(other: Complex): Complex {
        return new Complex(
            this.re * other.re - this.im * other.im,
            this.re * other.im + this.im * other.re
        );
    }

    toString(): string {
        return `${this.re} + ${this.im}i`;
    }
}

let num1 = new Complex(1, 2);
let num2 = new Complex(3, 4);
let sum = num1.add(num2);
let product = num1.multiply(num2);

console.log(`Sum: ${sum.toString()}`); // פלט: Sum: 4 + 6i
console.log(`Product: ${product.toString()}`); // פלט: Product: -5 + 10i
```

## צלילה עמוקה
באופן היסטורי, מספרים מרוכבים היו שנויים במחלוקת - אף שנקראו "דמיוניים" כדי לבטא את הספקיזם הראשוני. כיום, הם בסיסיים במתמטיקה ובמדע המודרניים.

חלופות למחלקה הפשוטה שלנו עשויות לכלול שימוש בספריות קיימות כמו `math.js` או `complex.js`, שמפורטות עם תכונות נוספות כמו פונקציות טריגונומטריות, העלאה בחזקה, וצירוף מרוכב.

הפרטים של היישום שלנו ב-TypeScript מסתכמים בהגדרת פעולות אריתמטיות. השיטה `add` פשוט מוסיפה את החלקים המתאימים. `multiply` מיישמת את שיטת FOIL שבשימוש באלגברה, תוך זכירה ש-`i^2 = -1`.

## ראה גם
לקריאה נוספת ומשאבים על מספרים מרוכבים והשימוש בהם בתכנות, בדוק:

- אלגברת מספרים מרוכבים ב-MDN: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt
- ספריית `math.js`: https://mathjs.org/docs/datatypes/complex_numbers.html
- ספריית `complex.js`: https://complex-js.github.io/complex.js/
