---
title:                "גירוד מספרים אקראיים"
date:                  2024-03-08T21:54:44.952121-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## מה ולמה?
יצירת מספרים אקראיים ב-Dart כוללת יצירת ערכים מספריים שאינם צפויים ושונים בכל הרצה. מתכנתים מנצלים את הפונקציונליות הזו מסיבות רבות, החל מחיקוי תרחישים מהעולם האמיתי בסביבות בדיקה ועד הפעלת מכניקות משחק והבטחת אבטחה דרך רנדומליות בפעולות קריפטוגרפיות.

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
