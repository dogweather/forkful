---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:44.952121-07:00
description: "\u05D9\u05E6\u05D9\u05E8\u05EA \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD\
  \ \u05D0\u05E7\u05E8\u05D0\u05D9\u05D9\u05DD \u05D1-Dart \u05DB\u05D5\u05DC\u05DC\
  \u05EA \u05D9\u05E6\u05D9\u05E8\u05EA \u05E2\u05E8\u05DB\u05D9\u05DD \u05DE\u05E1\
  \u05E4\u05E8\u05D9\u05D9\u05DD \u05E9\u05D0\u05D9\u05E0\u05DD \u05E6\u05E4\u05D5\
  \u05D9\u05D9\u05DD \u05D5\u05E9\u05D5\u05E0\u05D9\u05DD \u05D1\u05DB\u05DC \u05D4\
  \u05E8\u05E6\u05D4. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E0\u05E6\
  \u05DC\u05D9\u05DD \u05D0\u05EA \u05D4\u05E4\u05D5\u05E0\u05E7\u05E6\u05D9\u05D5\
  \u05E0\u05DC\u05D9\u05D5\u05EA \u05D4\u05D6\u05D5 \u05DE\u05E1\u05D9\u05D1\u05D5\
  \u05EA \u05E8\u05D1\u05D5\u05EA, \u05D4\u05D7\u05DC \u05DE\u05D7\u05D9\u05E7\u05D5\
  \u05D9 \u05EA\u05E8\u05D7\u05D9\u05E9\u05D9\u05DD\u2026"
lastmod: '2024-03-13T22:44:38.838460-06:00'
model: gpt-4-0125-preview
summary: "\u05D9\u05E6\u05D9\u05E8\u05EA \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05D0\
  \u05E7\u05E8\u05D0\u05D9\u05D9\u05DD \u05D1-Dart \u05DB\u05D5\u05DC\u05DC\u05EA\
  \ \u05D9\u05E6\u05D9\u05E8\u05EA \u05E2\u05E8\u05DB\u05D9\u05DD \u05DE\u05E1\u05E4\
  \u05E8\u05D9\u05D9\u05DD \u05E9\u05D0\u05D9\u05E0\u05DD \u05E6\u05E4\u05D5\u05D9\
  \u05D9\u05DD \u05D5\u05E9\u05D5\u05E0\u05D9\u05DD \u05D1\u05DB\u05DC \u05D4\u05E8\
  \u05E6\u05D4."
title: "\u05D2\u05D9\u05E8\u05D5\u05D3 \u05DE\u05E1\u05E4\u05E8\u05D9\u05DD \u05D0\
  \u05E7\u05E8\u05D0\u05D9\u05D9\u05DD"
weight: 12
---

## כיצד:
ספריית הליבה של Dart כוללת תמיכה ביצירת מספרים אקראיים עם המחלקה `Random` שנמצאת ב-`dart:math`. הנה דוגמה בסיסית:

```dart
import 'dart:math';

void main() {
  var rand = Random();
  int randomNumber = rand.nextInt(100); // מייצר מספר שלם אקראי בין 0 ל-99
  double randomDouble = rand.nextDouble(); // מייצר מספר כפול אקראי בין 0.0 ל-1.0
  print(randomNumber);
  print(randomDouble);
}
```

*פלט לדוגמה: (התוצאה תשתנה בכל פעם שהיא רצה)*

```
23
0.6722390975465775
```

עבור תרחישים הדורשים רנדומליות קריפטוגרפית, Dart מציעה את הבנאי `Random.secure`:

```dart
import 'dart:math';

void main() {
  var secureRand = Random.secure();
  int secureRandomNumber = secureRand.nextInt(100);
  print(secureRandomNumber);
}
```

*פלט לדוגמה: (התוצאה תשתנה בכל פעם שהיא רצה)*

```
45
```

אם אתה עובד על פרויקטים של Flutter או שאתה צריך רנדומליות מורכבת יותר, יתכן שתמצא את חבילת `faker` שימושית ליצירת מגוון רחב של נתונים אקראיים, כמו שמות, כתובות ותאריכים.

כדי להשתמש ב-`faker`, קודם כל, הוסף אותו לקובץ ה-`pubspec.yaml` שלך:

```yaml
dependencies:
  faker: ^2.0.0
```

לאחר מכן, ייבא והשתמש כמוצג:

```dart
import 'package:faker/faker.dart';

void main() {
  final faker = Faker();
  print(faker.person.name()); // מייצר שם אקראי
  print(faker.address.city()); // מייצר שם של עיר אקראית
}
```

*פלט לדוגמה:*

```
Josie Runolfsdottir
East Lysanne
```
