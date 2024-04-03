---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:41.248669-07:00
description: "\u05D8\u05D9\u05E4\u05D5\u05DC \u05D1\u05E9\u05D2\u05D9\u05D0\u05D5\u05EA\
  \ \u05D1-Dart \u05E7\u05E9\u05D5\u05E8 \u05DC\u05D6\u05D9\u05D4\u05D5\u05D9 \u05D5\
  \u05E0\u05D9\u05D4\u05D5\u05DC \u05D7\u05E8\u05D9\u05D2\u05D9\u05DD \u05E9\u05E2\
  \u05D5\u05DC\u05D9\u05DD \u05D1\u05DE\u05D4\u05DC\u05DA \u05D1\u05D9\u05E6\u05D5\
  \u05E2 \u05D4\u05EA\u05DB\u05E0\u05D9\u05EA \u05DB\u05D3\u05D9 \u05DC\u05E9\u05E4\
  \u05E8 \u05D0\u05EA \u05D4\u05D0\u05DE\u05D9\u05E0\u05D5\u05EA \u05D5\u05D4\u05E0\
  \u05D5\u05D7\u05D5\u05EA \u05D1\u05E9\u05D9\u05DE\u05D5\u05E9. \u05DE\u05EA\u05DB\
  \u05E0\u05EA\u05D9\u05DD \u05DE\u05D9\u05D9\u05E9\u05DE\u05D9\u05DD \u05D8\u05D9\
  \u05E4\u05D5\u05DC \u05D1\u05E9\u05D2\u05D9\u05D0\u05D5\u05EA \u05DB\u05D3\u05D9\
  \ \u05DC\u05DE\u05E0\u05D5\u05E2\u2026"
lastmod: '2024-03-13T22:44:38.858980-06:00'
model: gpt-4-0125-preview
summary: "\u05D8\u05D9\u05E4\u05D5\u05DC \u05D1\u05E9\u05D2\u05D9\u05D0\u05D5\u05EA\
  \ \u05D1-Dart \u05E7\u05E9\u05D5\u05E8 \u05DC\u05D6\u05D9\u05D4\u05D5\u05D9 \u05D5\
  \u05E0\u05D9\u05D4\u05D5\u05DC \u05D7\u05E8\u05D9\u05D2\u05D9\u05DD \u05E9\u05E2\
  \u05D5\u05DC\u05D9\u05DD \u05D1\u05DE\u05D4\u05DC\u05DA \u05D1\u05D9\u05E6\u05D5\
  \u05E2 \u05D4\u05EA\u05DB\u05E0\u05D9\u05EA \u05DB\u05D3\u05D9 \u05DC\u05E9\u05E4\
  \u05E8 \u05D0\u05EA \u05D4\u05D0\u05DE\u05D9\u05E0\u05D5\u05EA \u05D5\u05D4\u05E0\
  \u05D5\u05D7\u05D5\u05EA \u05D1\u05E9\u05D9\u05DE\u05D5\u05E9."
title: "\u05D8\u05D9\u05E4\u05D5\u05DC \u05D1\u05E9\u05D2\u05D9\u05D0\u05D5\u05EA"
weight: 16
---

## מה ולמה?
טיפול בשגיאות ב-Dart קשור לזיהוי וניהול חריגים שעולים במהלך ביצוע התכנית כדי לשפר את האמינות והנוחות בשימוש. מתכנתים מיישמים טיפול בשגיאות כדי למנוע קריסות ולספק משוב מובן למשתמשים, לצורך חוויית יישום חלקה ובטוחה יותר.

## איך לעשות זאת:
Dart תומך בשני סוגים של שגיאות: שגיאות *בזמן קומפילציה* ושגיאות *בזמן ריצה*. שגיאות בזמן קומפילציה מתגלות על ידי האנליזטור של Dart לפני הרצת הקוד, בעוד שגיאות בזמן ריצה, או חריגים, נוצרות במהלך הביצוע. הנה איך לטפל בחריגים ב-Dart:

### Try-Catch
השתמשו ב-`try-catch` כדי לתפוס חריגים ולמנוע מהם לקרוס את היישום שלכם:

```dart
try {
  var result = 100 ~/ 0; // ניסיון חלוקה באפס, מזריק חריג
} catch (e) {
  print('נתפס חריג: $e'); // מטפל בחריג
}
```
פלט לדוגמה: `נתפס חריג: IntegerDivisionByZeroException`

### חריג ספציפי
כדי לטפל בחריגים ספציפיים, יש לציין את החריג אחרי ה-`catch`:

```dart
try {
  var result = 100 ~/ 0;
} on IntegerDivisionByZeroException {
  print('לא ניתן לחלק באפס.'); // מטפל באופן ספציפי בחריגי חלוקה באפס
}
```
פלט לדוגמה: `לא ניתן לחלק באפס.`

### מעקב אחר ריצה
כדי לקבל מעקב אחר ריצה לצורך ניפוי באגים, השתמשו בפרמטר שני בבלוק ה-catch:

```dart
try {
  var result = 100 ~/ 0;
} catch (e, s) {
  print('חריג: $e');
  print('מעקב אחר ריצה: $s'); // מדפיס מעקב אחר ריצה לצורך ניפוי באגים
}
```

### Finally
השתמשו ב-`finally` כדי לבצע קוד אחרי try/catch, ללא תלות אם נזרק חריג:

```dart
try {
  var result = 100 ~/ 0;
} catch (e) {
  print('נתפס חריג: $e');
} finally {
  print('זה תמיד מתבצע.'); // קוד ניקוי או שלבים סופיים
}
```
פלט לדוגמה:
```
נתפס חריג: IntegerDivisionByZeroException
זה תמיד מתבצע.
```

### ספריות צד שלישי
אף על פי שספריית הליבה של Dart חזקה לטיפול בשגיאות, ניתן גם להשתמש בחבילות צד שלישי כמו `dartz` לתכנות פונקציונלי שמציג רעיונות כמו `Either` ו-`Option` שניתן להשתמש בהם לטיפול בשגיאות. הנה דוגמה לשימוש ב-`dartz` לטיפול בשגיאות:

1. הוסיפו את `dartz` לקובץ `pubspec.yaml` שלכם תחת תלויות:
```yaml
dependencies:
  dartz: ^0.10.0
```

2. השתמשו ב-`Either` לטיפול חסד בשגיאות בקוד Dart שלכם:
```dart
import 'package:dartz/dartz.dart';

Either<String, int> divide(int dividend, int divisor) {
  if (divisor == 0) {
    return Left('לא ניתן לחלק באפס.');
  } else {
    return Right(dividend ~/ divisor);
  }
}

void main() {
  final result = divide(100, 0);
  result.fold(
    (left) => print('שגיאה: $left'), 
    (right) => print('תוצאה: $right')
  );
}
```
פלט לדוגמה: `שגיאה: לא ניתן לחלק באפס.`

החלק `Left` בדרך כלל מייצג את השגיאה, והחלק `Right` מייצג הצלחה. הדפוס הזה מאפשר טיפול בשגיאות באופן פונקציונלי יותר, מציע נקיון ושליטה על ניהול השגיאות.
