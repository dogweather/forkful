---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:56.442701-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: Dart \u05E2\u05E6\
  \u05DE\u05D5 \u05D0\u05D9\u05E0\u05D5 \u05DB\u05D5\u05DC\u05DC \u05E1\u05E4\u05E8\
  \u05D9\u05D4 \u05DE\u05D5\u05D1\u05E0\u05D9\u05EA \u05DC\u05DE\u05E1\u05E4\u05E8\
  \u05D9\u05DD \u05DE\u05E8\u05D5\u05DB\u05D1\u05D9\u05DD, \u05D3\u05D1\u05E8 \u05D4\
  \u05D3\u05D5\u05E8\u05E9 \u05D0\u05D5 \u05D0\u05EA \u05D9\u05D9\u05E9\u05D5\u05DD\
  \ \u05DE\u05D7\u05DC\u05E7\u05EA \u05DE\u05E1\u05E4\u05E8 \u05DE\u05E8\u05D5\u05DB\
  \u05D1 \u05DE\u05D5\u05EA\u05D0\u05DE\u05EA \u05D0\u05D9\u05E9\u05D9\u05EA \u05D0\
  \u05D5 \u05D0\u05EA \u05D4\u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\u05E1\u05E4\u05E8\
  \u05D9\u05D9\u05D4 \u05E9\u05DC \u05E6\u05D3 \u05E9\u05DC\u05D9\u05E9\u05D9. \u05D1\
  \u05D7\u05D9\u05E8\u05D4\u2026"
lastmod: '2024-03-13T22:44:38.834980-06:00'
model: gpt-4-0125-preview
summary: "Dart \u05E2\u05E6\u05DE\u05D5 \u05D0\u05D9\u05E0\u05D5 \u05DB\u05D5\u05DC\
  \u05DC \u05E1\u05E4\u05E8\u05D9\u05D4 \u05DE\u05D5\u05D1\u05E0\u05D9\u05EA \u05DC\
  \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05DE\u05E8\u05D5\u05DB\u05D1\u05D9\u05DD\
  , \u05D3\u05D1\u05E8 \u05D4\u05D3\u05D5\u05E8\u05E9 \u05D0\u05D5 \u05D0\u05EA \u05D9\
  \u05D9\u05E9\u05D5\u05DD \u05DE\u05D7\u05DC\u05E7\u05EA \u05DE\u05E1\u05E4\u05E8\
  \ \u05DE\u05E8\u05D5\u05DB\u05D1 \u05DE\u05D5\u05EA\u05D0\u05DE\u05EA \u05D0\u05D9\
  \u05E9\u05D9\u05EA \u05D0\u05D5 \u05D0\u05EA \u05D4\u05E9\u05D9\u05DE\u05D5\u05E9\
  \ \u05D1\u05E1\u05E4\u05E8\u05D9\u05D9\u05D4 \u05E9\u05DC \u05E6\u05D3 \u05E9\u05DC\
  \u05D9\u05E9\u05D9."
title: "\u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD \u05DE\u05E1\u05E4\u05E8\u05D9\
  \u05DD \u05DE\u05E8\u05D5\u05DB\u05D1\u05D9\u05DD"
weight: 14
---

## איך לעשות:
Dart עצמו אינו כולל ספריה מובנית למספרים מרוכבים, דבר הדורש או את יישום מחלקת מספר מרוכב מותאמת אישית או את השימוש בספרייה של צד שלישי. בחירה פופולרית למשימות חישוב עילאיות, שכוללת תמיכה במספרים מרוכבים, היא `package:scidart`.

### יישום מחלקת מספר מרוכב בסיסית
לפעולות פשוטות, ניתן בקלות להגדיר מחלקת מספר מרוכב משלך:

```dart
class Complex {
  final double real;
  final double imaginary;

  Complex(this.real, this.imaginary);

  // הוספת שני מספרים מרוכבים
  Complex operator +(Complex other) {
    return Complex(real + other.real, imaginary + other.imaginary);
  }

  // ייצוג מחרוזת לניפוי באגים בקלות
  @override
  String toString() => '${real} + ${imaginary}i';
}

void main() {
  var number1 = Complex(3, 4);
  var number2 = Complex(1, 2);

  var sum = number1 + number2;
  print(sum);  // 4.0 + 6.0i
}
```

### שימוש ב-SciDart לפעולות מתקדמות
לפעולות מורכבות יותר או כאשר הביצועים קריטיים, חבילת `package:scidart` מציעה תמיכה מקיפה למספרים מרוכבים בין היתר לתפקודי חישוב מדעי. תחילה, הוסף את SciDart לקובץ pubspec.yaml שלך:

```yaml
dependencies:
  scidart: ^0.0.1-dev.9
```

הנה איך לבצע פעולות בסיסיות עם מספרים מרוכבים באמצעות SciDart:

```dart
import 'package:scidart/numdart.dart';

void main() {
  // יצירת מספרים מרוכבים
  var complexNum1 = Complex(real: 5, imaginary: 3);
  var complexNum2 = Complex(real: 2, imaginary: 7);

  // הוספה
  var sum = complexAdd(complexNum1, complexNum2);
  
  // כפל
  var product = complexMultiply(complexNum1, complexNum2);

  print('Sum: ${sum.toString()}');  // Sum: Complex(real: 7.0, imaginary: 10.0)
  print('Product: ${product.toString()}');  // Product: Complex(real: -11.0, imaginary: 41.0)
}
```

דוגמאות אלו מדגימות את התמודדות הבסיסית והשימוש במספרים מרוכבים ב-Dart, דרך יישום מותאם אישית ובאמצעות ספריית SciDart, מדגישות את הגמישות והעוצמה של Dart למשימות חישוב מדעיות.
