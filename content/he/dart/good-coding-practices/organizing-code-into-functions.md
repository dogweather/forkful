---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:47.963163-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: \u05D1-Dart, \u05DE\
  \u05D2\u05D3\u05D9\u05E8\u05D9\u05DD \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D4\
  \ \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA \u05D4\u05DE\u05D9\u05DC\u05EA \u05D4\
  \u05DE\u05E4\u05EA\u05D7 `void` \u05D0\u05DD \u05D4\u05D9\u05D0 \u05DC\u05D0 \u05DE\
  \u05D7\u05D6\u05D9\u05E8\u05D4 \u05E2\u05E8\u05DA, \u05D0\u05D5 \u05DE\u05E6\u05D9\
  \u05D9\u05E0\u05D9\u05DD \u05D0\u05EA \u05E1\u05D5\u05D2 \u05D4\u05E2\u05E8\u05DA\
  \ \u05E9\u05D4\u05D9\u05D0 \u05DE\u05D7\u05D6\u05D9\u05E8\u05D4 \u05D0\u05D7\u05E8\
  \u05EA. \u05D4\u05E0\u05D4 \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D4 \u05E4\u05E9\
  \u05D5\u05D8\u05D4 \u05E9\u05DE\u05D3\u05E4\u05D9\u05E1\u05D4\u2026"
lastmod: '2024-03-13T22:44:38.855526-06:00'
model: gpt-4-0125-preview
summary: "\u05D1-Dart, \u05DE\u05D2\u05D3\u05D9\u05E8\u05D9\u05DD \u05E4\u05D5\u05E0\
  \u05E7\u05E6\u05D9\u05D4 \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA \u05D4\u05DE\
  \u05D9\u05DC\u05EA \u05D4\u05DE\u05E4\u05EA\u05D7 `void` \u05D0\u05DD \u05D4\u05D9\
  \u05D0 \u05DC\u05D0 \u05DE\u05D7\u05D6\u05D9\u05E8\u05D4 \u05E2\u05E8\u05DA, \u05D0\
  \u05D5 \u05DE\u05E6\u05D9\u05D9\u05E0\u05D9\u05DD \u05D0\u05EA \u05E1\u05D5\u05D2\
  \ \u05D4\u05E2\u05E8\u05DA \u05E9\u05D4\u05D9\u05D0 \u05DE\u05D7\u05D6\u05D9\u05E8\
  \u05D4 \u05D0\u05D7\u05E8\u05EA."
title: "\u05D0\u05E8\u05D2\u05D5\u05DF \u05E7\u05D5\u05D3 \u05DC\u05EA\u05D5\u05DA\
  \ \u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\u05EA"
weight: 18
---

## איך לעשות:


### פונקציה בסיסית
ב-Dart, מגדירים פונקציה באמצעות המילת המפתח `void` אם היא לא מחזירה ערך, או מציינים את סוג הערך שהיא מחזירה אחרת. הנה פונקציה פשוטה שמדפיסה הודעת ברכה:

```dart
void greet(String name) {
  print('Hello, $name!');
}

void main() {
  greet('Alice');  // פלט: Hello, Alice!
}
```

### החזרת ערך
פונקציות יכולות להחזיר ערכים. הדוגמה הבאה לוקחת שני שלמים כקלט ומחזירה את הסכום שלהם:

```dart
int add(int a, int b) {
  return a + b;
}

void main() {
  var sum = add(5, 3);
  print(sum);  // פלט: 8
}
```

### פונקציות אנונימיות
-Dart תומך בפונקציות אנונימיות (הידועות גם כביטויי למבדא או סגירות), אשר יכולות להיות שימושיות לפונקציונליות קצרות ומהירות. הנה איך להשתמש בפונקציה אנונימית עם המתודה `forEach` של רשימה:

```dart
void main() {
  var fruits = ['apple', 'banana', 'cherry'];
  fruits.forEach((item) {
    print(item);
  });
  // פלט:
  // apple
  // banana
  // cherry
}
```

### תחביר חץ לפונקציות עם ביטוי יחיד
עבור פונקציות שמכילות ביטוי יחיד בלבד, Dart מציע תחביר קצרצר באמצעות הסימון "חץ" (`=>`). זה מועיל במיוחד עבור פונקציות קצרות או העברת פונקציות כארגומנטים:

```dart
int square(int num) => num * num;

void main() {
  print(square(4));  // פלט: 16
}
```

### שימוש בספריות של צד שלישי
לפונקציונליות מורכבות יותר או מתמחות, תכנתי Dart לעיתים קרובות מסתמכים על ספריות של צד שלישי. קח לדוגמה את הספרייה `http` לביצוע בקשות HTTP. ראשית, הוסף את `http` לקובץ pubspec.yaml שלך תחת תלות:

```
dependencies:
  http: ^0.13.3
```

לאחר מכן, ניתן להשתמש בה לשליפת נתונים מהרשת:

```dart
import 'package:http/http.dart' as http;

Future<void> fetchUserData() async {
  var response = await http.get(Uri.parse('https://api.example.com/users/1'));
  print(response.body);
}

void main() {
  fetchUserData();
  // פלט משוער: נתוני JSON של המשתמש. הפלט המדויק יהיה תלוי בתגובת ה-API.
}
```

זכור, כאשר אתה מארגן את הקוד שלך ב-Dart לתוך פונקציות, חשוב על נתמידות, קריאות ועקרון האחריות היחידה. זה לא רק מנקה את הקוד שלך אלא גם הופך אותו לקל יותר להבנה ולתחזוקה עבור אחרים (והאני העתידי שלך).
