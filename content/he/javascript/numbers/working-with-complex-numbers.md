---
date: 2024-01-26 04:43:04.211504-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1-JavaScript\
  \ \u05D0\u05D9\u05DF \u05EA\u05DE\u05D9\u05DB\u05D4 \u05E4\u05E0\u05D9\u05DE\u05D9\
  \u05EA \u05D1\u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05DE\u05D5\u05E8\u05DB\u05D1\
  \u05D9\u05DD, \u05D0\u05DA \u05D0\u05E4\u05E9\u05E8 \u05DC\u05E7\u05E4\u05DC \u05D0\
  \u05EA \u05D4\u05E9\u05E8\u05D5\u05D5\u05DC\u05D9\u05DD \u05D5\u05DC\u05D4\u05EA\
  \u05DE\u05D5\u05D3\u05D3 \u05E2\u05DD \u05D6\u05D4 \u05D1\u05D0\u05DE\u05E6\u05E2\
  \u05D5\u05EA \u05D0\u05D5\u05D1\u05D9\u05D9\u05E7\u05D8\u05D9\u05DD \u05D5\u05DE\
  \u05EA\u05DE\u05D8\u05D9\u05E7\u05D4. \u05D4\u05E0\u05D4 \u05D3\u05D5\u05D2\u05DE\
  \u05D0 \u05DE\u05D4\u05D9\u05E8\u05D4."
lastmod: '2024-03-13T22:44:39.964664-06:00'
model: gpt-4-0125-preview
summary: "\u05D1-JavaScript \u05D0\u05D9\u05DF \u05EA\u05DE\u05D9\u05DB\u05D4 \u05E4\
  \u05E0\u05D9\u05DE\u05D9\u05EA \u05D1\u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05DE\
  \u05D5\u05E8\u05DB\u05D1\u05D9\u05DD, \u05D0\u05DA \u05D0\u05E4\u05E9\u05E8 \u05DC\
  \u05E7\u05E4\u05DC \u05D0\u05EA \u05D4\u05E9\u05E8\u05D5\u05D5\u05DC\u05D9\u05DD\
  \ \u05D5\u05DC\u05D4\u05EA\u05DE\u05D5\u05D3\u05D3 \u05E2\u05DD \u05D6\u05D4 \u05D1\
  \u05D0\u05DE\u05E6\u05E2\u05D5\u05EA \u05D0\u05D5\u05D1\u05D9\u05D9\u05E7\u05D8\u05D9\
  \u05DD \u05D5\u05DE\u05EA\u05DE\u05D8\u05D9\u05E7\u05D4."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD \u05DE\u05E1\u05E4\u05E8\u05D9\
  \u05DD \u05DE\u05E8\u05D5\u05DB\u05D1\u05D9\u05DD"
weight: 14
---

## איך לעשות:
ב-JavaScript אין תמיכה פנימית במספרים מורכבים, אך אפשר לקפל את השרוולים ולהתמודד עם זה באמצעות אובייקטים ומתמטיקה. הנה דוגמא מהירה.

```javascript
class ComplexNumber {
  constructor(real, imaginary) {
    this.real = real;
    this.imaginary = imaginary;
  }

  add(other) {
    return new ComplexNumber(this.real + other.real, this.imaginary + other.imaginary);
  }

  // ...הוסף עוד מתודות (חיסור, כפל, חילוק) לפי הצורך

  toString() {
    return `${this.real} + ${this.imaginary}i`;
  }
}

const a = new ComplexNumber(1, 2);
const b = new ComplexNumber(3, 4);
const result = a.add(b);

console.log(`תוצאה: ${result}`); // תוצאה: 4 + 6i
```

## צלילה עמוקה
מספרים מורכבים הם בשימוש מהמאה ה-16, הודות למתמטיקאי האיטלקי ג'רולמו קרדנו. הם הפכו חיוניים במגוון תחומים, כמו הנדסה ופיזיקה. בתכנות המודרני, הם מפתח לסימולציות ואלגוריתמים הדורשים רב מימדיות.

עכשיו, JavaScript אינו מצויד למספרים מורכבים כברירת מחדל. אבל מעבר לאפשרות ה-DIY, אפשר להשתמש בספריות מתמטיות כמו math.js או numeric.js. הן מציעות את הכוח להרמת משקולות כבדה יותר של מספרים מורכבים, ומוסיפות יתרונות כמו יותר פעולות, חישוב גודל, ואיתור ארגומנטים.

מתחת למכסה, כאשר אתם מבצעים פעולות עם מספרים מורכבים, זה כמו לנהל שני מספרים נפרדים שקשורים זה לזה. פעולות של חיבור וחיסור הן עניינים ישרים – מתאימים ממשי לממשי, דמיוני לדמיוני. כפל וחלוקה מתבלים עם ריקודים של איברים מתקלפים ודורשים תשומת לב נוספת.

## ראה גם
- MDN Web Docs על JavaScript: https://developer.mozilla.org/en-US/docs/Web/JavaScript/A_re-introduction_to_JavaScript
- Math.js, ספריית מתמטיקה הכוללת מספרים מורכבים: https://mathjs.org/docs/datatypes/complex_numbers.html
- Numeric.js, ספרייה נוספת: http://numericjs.com/documentation.html
- צלילה עמוקה יותר בנושא מספרים מורכבים (מתמקד במתמטיקה): https://mathworld.wolfram.com/ComplexNumber.html
