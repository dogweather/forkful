---
title:                "עבודה עם מספרים מרוכבים"
date:                  2024-01-26T04:43:04.211504-07:00
model:                 gpt-4-0125-preview
simple_title:         "עבודה עם מספרים מרוכבים"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/javascript/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## מה ולמה?
מספרים מורכבים הם מספרים עם חלק ממשי וחלק דמיוני (כמו 3 + 4i). הם מופיעים במגוון בעיות תכנות, במיוחד בעיבוד אותות, חישוב קוונטי ופתרון משוואות פולינומיאליות. מתכנתים מתעסקים בהם כדי לתפקד באופן יעיל במשימות מסוג זה.

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
