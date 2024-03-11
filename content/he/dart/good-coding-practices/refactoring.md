---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:17.081086-07:00
description: "\u05E8\u05E4\u05E7\u05D8\u05D5\u05E8\u05D9\u05E0\u05D2 \u05D1-Dart \u05D4\
  \u05D5\u05D0 \u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05DC \u05E9\u05D9\u05E0\u05D5\
  \u05D9 \u05DE\u05D1\u05E0\u05D4 \u05E7\u05D5\u05D3 \u05E7\u05D9\u05D9\u05DD \u05DC\
  \u05DC\u05D0 \u05E9\u05D9\u05E0\u05D5\u05D9 \u05D4\u05EA\u05E0\u05D4\u05D2\u05D5\
  \u05EA\u05D5 \u05D4\u05D7\u05D9\u05E6\u05D5\u05E0\u05D9\u05EA, \u05D1\u05DE\u05D8\
  \u05E8\u05D4 \u05DC\u05E9\u05E4\u05E8 \u05D0\u05EA \u05DE\u05D1\u05E0\u05D4\u05D5\
  \ \u05D4\u05E4\u05E0\u05D9\u05DE\u05D9, \u05E7\u05E8\u05D9\u05D0\u05D5\u05EA\u05D5\
  \ \u05D5\u05EA\u05D7\u05D6\u05D5\u05E7\u05EA\u05D5. \u05DE\u05EA\u05DB\u05E0\u05EA\
  \u05D9\u05DD \u05DC\u05E2\u05D9\u05EA\u05D9\u05DD \u05E7\u05E8\u05D5\u05D1\u05D5\
  \u05EA \u05E2\u05D5\u05E9\u05D9\u05DD\u2026"
lastmod: '2024-03-11T00:14:12.288985-06:00'
model: gpt-4-0125-preview
summary: "\u05E8\u05E4\u05E7\u05D8\u05D5\u05E8\u05D9\u05E0\u05D2 \u05D1-Dart \u05D4\
  \u05D5\u05D0 \u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05DC \u05E9\u05D9\u05E0\u05D5\
  \u05D9 \u05DE\u05D1\u05E0\u05D4 \u05E7\u05D5\u05D3 \u05E7\u05D9\u05D9\u05DD \u05DC\
  \u05DC\u05D0 \u05E9\u05D9\u05E0\u05D5\u05D9 \u05D4\u05EA\u05E0\u05D4\u05D2\u05D5\
  \u05EA\u05D5 \u05D4\u05D7\u05D9\u05E6\u05D5\u05E0\u05D9\u05EA, \u05D1\u05DE\u05D8\
  \u05E8\u05D4 \u05DC\u05E9\u05E4\u05E8 \u05D0\u05EA \u05DE\u05D1\u05E0\u05D4\u05D5\
  \ \u05D4\u05E4\u05E0\u05D9\u05DE\u05D9, \u05E7\u05E8\u05D9\u05D0\u05D5\u05EA\u05D5\
  \ \u05D5\u05EA\u05D7\u05D6\u05D5\u05E7\u05EA\u05D5. \u05DE\u05EA\u05DB\u05E0\u05EA\
  \u05D9\u05DD \u05DC\u05E2\u05D9\u05EA\u05D9\u05DD \u05E7\u05E8\u05D5\u05D1\u05D5\
  \u05EA \u05E2\u05D5\u05E9\u05D9\u05DD\u2026"
title: "\u05E9\u05D9\u05E4\u05D5\u05E5 \u05E7\u05D5\u05D3"
---

{{< edit_this_page >}}

## מה ולמה?

רפקטורינג ב-Dart הוא תהליך של שינוי מבנה קוד קיים ללא שינוי התנהגותו החיצונית, במטרה לשפר את מבנהו הפנימי, קריאותו ותחזוקתו. מתכנתים לעיתים קרובות עושים רפקטורינג כדי להפוך את הקוד לנקי, קל יותר להבנה או יעיל יותר, מה שמקל על שינויים עתידיים ומפחית את הסבירות לבאגים.

## איך לבצע:

### דוגמה 1: שינוי שמות וחילוץ שיטות

לפני רפקטורינג, ייתכן שיש לכם חתיכת קוד המערבת רמות שונות של אבסטרקציה או אחריותים, כמו חישוב הנחה ולאחר מכן החלתה:

```dart
void main() {
  var price = 100.0;
  var discount = 0.2;
  var finalPrice = price - (price * discount);
  print("Final price: $finalPrice");
}
```

**פלט:**
```
Final price: 80.0
```

לאחר רפקטורינג, תוכלו לחלץ את חישוב ההנחה לשיטה משלה ולתת לה שם משמעותי:

```dart
void main() {
  var price = 100.0;
  var discount = 0.2;
  var finalPrice = calculateFinalPrice(price, discount);
  print("Final price: $finalPrice");
}

double calculateFinalPrice(double price, double discount) {
  return price - (price * discount);
}
```

**פלט:**
```
Final price: 80.0
```

על ידי חילוץ החישוב לשיטה, כעת יש לכם פעולה המוגדרת בבירור שניתן להשתמש בה מחדש, לבדוק באופן עצמאי ולשנות בקלות.

### דוגמה 2: פישוט ביטויי תנאי

לפני רפקטורינג, ייתכן שההצהרות התנאיות מורכבות או קשות לקריאה מדי:

```dart
void main() {
  var customerType = "regular";
  double discount;
  
  if (customerType == "regular") {
    discount = 0.05;
  } else if (customerType == "member") {
    discount = 0.1;
  } else {
    discount = 0.0;
  }

  print("Discount: $discount");
}
```

**פלט:**
```
Discount: 0.05
```

לאחר רפקטורינג, שקלו להשתמש ב-map למבנה ברור יותר ועדכונים או הרחבות קלים יותר לסוגי לקוחות והנחות:

```dart
void main() {
  var customerType = "regular";
  var discounts = {
    "regular": 0.05,
    "member": 0.1,
    "none": 0.0,
  };

  var discount = discounts[customerType] ?? 0.0;
  print("Discount: $discount");
}
```

**פלט:**
```
Discount: 0.05
```

רפקטורינג זה לא רק הופך את הקוד לתמציתי יותר אלא גם מאגד את הלוגיקה לקביעת ההנחות בדרך שקלה יותר להבנה ותחזוקה.

### ספריות צד שלישי לרפקטורינג

כאשר מדובר ברפקטורינג ב-Dart, בעיקר בתוך אפליקציות Flutter, חבילת ה-[Dart DevTools](https://dart.dev/tools/dart-devtools) היא בלתי נמנעת. היא כוללת כלים לביצועים, בודק ווידג'ט ומנפה מקור. למרות שזו לא ספריית צד שלישי, משתמשים ב-Dart DevTools לעיתים קרובות לצד ספריות כמו `flutter_bloc` לניהול מדיניות בצורה נקייה שתורמת לרפקטורינג לשיפור המודולריות וקריאות. לצערנו, בגלל היקף כניסה זו, דוגמאות קוד מסוימות באמצעות ספריות צד שלישי לא יוענקו כאן, אך מפתחים מוזמנים לחקור את כלים אלו כדי לשפר את תהליך הרפקטורינג באפליקציות Dart/Flutter שלהם.
