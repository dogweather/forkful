---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:08:34.361360-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: Google Apps Script\
  \ \u05DC\u05D0 \u05DB\u05D5\u05DC\u05DC \u05EA\u05DE\u05D9\u05DB\u05D4 \u05DE\u05D5\
  \u05D1\u05E0\u05D9\u05EA \u05E2\u05D1\u05D5\u05E8 \u05DE\u05E1\u05E4\u05E8\u05D9\
  \u05DD \u05DE\u05E8\u05D5\u05DB\u05D1\u05D9\u05DD, \u05DE\u05D4 \u05E9\u05DE\u05D7\
  \u05D9\u05D9\u05D1 \u05D0\u05EA \u05D9\u05D9\u05E9\u05D5\u05DD \u05E4\u05D5\u05E0\
  \u05E7\u05E6\u05D9\u05D5\u05E0\u05DC\u05D9\u05D5\u05EA \u05DE\u05D5\u05EA\u05D0\u05DE\
  \u05EA \u05D0\u05D9\u05E9\u05D9\u05EA. \u05DC\u05D4\u05DC\u05DF \u05DE\u05D1\u05E0\
  \u05D4 \u05D1\u05E1\u05D9\u05E1\u05D9 \u05DC\u05D8\u05D9\u05E4\u05D5\u05DC \u05D1\
  \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD\u2026"
lastmod: '2024-03-13T22:44:38.554423-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script \u05DC\u05D0 \u05DB\u05D5\u05DC\u05DC \u05EA\u05DE\u05D9\
  \u05DB\u05D4 \u05DE\u05D5\u05D1\u05E0\u05D9\u05EA \u05E2\u05D1\u05D5\u05E8 \u05DE\
  \u05E1\u05E4\u05E8\u05D9\u05DD \u05DE\u05E8\u05D5\u05DB\u05D1\u05D9\u05DD, \u05DE\
  \u05D4 \u05E9\u05DE\u05D7\u05D9\u05D9\u05D1 \u05D0\u05EA \u05D9\u05D9\u05E9\u05D5\
  \u05DD \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05E0\u05DC\u05D9\u05D5\u05EA\
  \ \u05DE\u05D5\u05EA\u05D0\u05DE\u05EA \u05D0\u05D9\u05E9\u05D9\u05EA."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD \u05DE\u05E1\u05E4\u05E8\u05D9\
  \u05DD \u05DE\u05E8\u05D5\u05DB\u05D1\u05D9\u05DD"
weight: 14
---

## איך לעשות:
Google Apps Script לא כולל תמיכה מובנית עבור מספרים מרוכבים, מה שמחייב את יישום פונקציונליות מותאמת אישית. להלן מבנה בסיסי לטיפול במספרים מרוכבים, כולל חיבור, חיסור, וכפל.

```javascript
// הגדרת בנאי עבור מספרים מרוכבים
function Complex(real, imag) {
  this.real = real;
  this.imag = imag;
}

// מתודה לחיבור שני מספרים מרוכבים
Complex.prototype.add = function(other) {
  return new Complex(this.real + other.real, this.imag + other.imag);
};

// מתודה לחיסור שני מספרים מרוכבים
Complex.prototype.subtract = function(other) {
  return new Complex(this.real - other.real, this.imag - other.imag);
};

// מתודה לכפל שני מספרים מרוכבים
Complex.prototype.multiply = function(other) {
  return new Complex(
    this.real * other.real - this.imag * other.imag,
    this.real * other.imag + this.imag * other.real
  );
};

// דוגמא לשימוש
var num1 = new Complex(3, 4);
var num2 = new Complex(1, 2);

// לחבר שני מספרים מרוכבים
var sum = num1.add(num2);
console.log(`סכום: ${sum.real} + ${sum.imag}i`); // סכום: 4 + 6i

// לחסר שני מספרים מרוכבים
var difference = num1.subtract(num2);
console.log(`הפרש: ${difference.real} + ${difference.imag}i`); // הפרש: 2 + 2i

// לכפול שני מספרים מרוכבים
var product = num1.multiply(num2);
console.log(`מכפלה: ${product.real} + ${product.imag}i`); // מכפלה: -5 + 10i
```

## טבילה עמוקה:
המושג של מספרים מרוכבים חוזר למאה ה-16, אך העבודה של מתמטיקאים כמו אוילר וגאוס חיזקה את מקומם במתמטיקה. למרות השימושיות שלהם, תמיכה ישירה במספרים מרוכבים אינה קיימת ב-JavaScript או, בהרחבה, ב-Google Apps Script. היעדר תמיכה טבעתי אומר שפעולות על מספרים מרוכבים צריכים להתבצע באופן ידני, כפי שהודגם. עם זאת, זו מספקת הזדמנות למידה טובה ופונקציונליות מספקת עבור צרכים בסיסיים, לעבודה חישובית כבדה הדורשת שימוש במספרים מרוכבים, יכול להיות שכדאי לשקול להיעזר בסביבות תכנות אחרות המתאימות יותר לחישובים מתמטיים, כמו Python עם NumPy, המציעות פעולות מובנות ומאוד מיועלות לטיפול במספרים מרוכבים. עם זאת, הבנה ויישום של פעולות בסיסיות ב-Google Apps Script היא תרגול שימושי לאלו המעוניינים להרחיב את יכולות התכנות שלהם וליישמם במגוון רחב של הקשרים.
