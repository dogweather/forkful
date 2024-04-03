---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:58:33.615474-07:00
description: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05D8\u05E1\u05D8\u05D9\u05DD \u05D1\
  -Dart \u05DB\u05D5\u05DC\u05DC\u05EA \u05D9\u05E6\u05D9\u05E8\u05EA \u05DE\u05E7\
  \u05E8\u05D9 \u05D1\u05D3\u05D9\u05E7\u05D4 \u05DC\u05D0\u05D9\u05DE\u05D5\u05EA\
  \ \u05D0\u05D5\u05D8\u05D5\u05DE\u05D8\u05D9 \u05E9\u05D7\u05DC\u05E7\u05D9\u05DD\
  \ \u05E9\u05D5\u05E0\u05D9\u05DD \u05E9\u05DC \u05D4\u05EA\u05D5\u05DB\u05E0\u05D9\
  \u05EA \u05E9\u05DC\u05DA \u05E2\u05D5\u05D1\u05D3\u05D9\u05DD \u05DB\u05E6\u05E4\
  \u05D5\u05D9. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\
  \u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9 \u05DC\u05D4\u05D1\u05D8\u05D9\u05D7\
  \ \u05E9\u05D4\u05E7\u05D5\u05D3 \u05E9\u05DC\u05D4\u05DD \u05D0\u05DE\u05D9\u05DF\
  \ \u05D5\u05D7\u05D5\u05E4\u05E9\u05D9\u2026"
lastmod: '2024-03-13T22:44:38.852063-06:00'
model: gpt-4-0125-preview
summary: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05D8\u05E1\u05D8\u05D9\u05DD \u05D1-Dart\
  \ \u05DB\u05D5\u05DC\u05DC\u05EA \u05D9\u05E6\u05D9\u05E8\u05EA \u05DE\u05E7\u05E8\
  \u05D9 \u05D1\u05D3\u05D9\u05E7\u05D4 \u05DC\u05D0\u05D9\u05DE\u05D5\u05EA \u05D0\
  \u05D5\u05D8\u05D5\u05DE\u05D8\u05D9 \u05E9\u05D7\u05DC\u05E7\u05D9\u05DD \u05E9\
  \u05D5\u05E0\u05D9\u05DD \u05E9\u05DC \u05D4\u05EA\u05D5\u05DB\u05E0\u05D9\u05EA\
  \ \u05E9\u05DC\u05DA \u05E2\u05D5\u05D1\u05D3\u05D9\u05DD \u05DB\u05E6\u05E4\u05D5\
  \u05D9."
title: "\u05DB\u05EA\u05D9\u05D1\u05EA \u05D1\u05D3\u05D9\u05E7\u05D5\u05EA"
weight: 36
---

## מה ולמה?

כתיבת טסטים ב-Dart כוללת יצירת מקרי בדיקה לאימות אוטומטי שחלקים שונים של התוכנית שלך עובדים כצפוי. מתכנתים עושים זאת כדי להבטיח שהקוד שלהם אמין וחופשי מפגמים, מה שמקל על עדכונים ושינויים מבניים תוך מניעת רגרסיות.

## איך לעשות:

ב-Dart, החבילה `test` משמשת לעיתים קרובות לכתיבת טסטים. ראשית, הוסף את חבילת ה`test` ל`pubspec.yaml` שלך:

```yaml
dev_dependencies:
  test: ^1.0.0
```

לאחר מכן, כתוב טסט לפונקציה פשוטה. נניח שיש לך פונקציה שמוסיפה שני מספרים:

```dart
int add(int a, int b) {
  return a + b;
}
```

לאחר מכן, צור קובץ בשם `add_test.dart` בתיקייה `test` וכתוב את מקרה הבדיקה שלך:

```dart
import 'package:test/test.dart';
import '../lib/add.dart'; // נניח שהפונקציה `add` שלך נמצאת בlib/add.dart

void main() {
  test('מוסיפה שני מספרים', () {
    var expected = 3;
    expect(add(1, 2), equals(expected));
  });
}
```

להרצת הטסטים, השתמש בפקודת Dart:

```bash
$ dart test
```

פלט לדוגמה עשוי להיראות כך:

```
00:01 +1: כל הטסטים עברו!
```

### שימוש בספרייה צד שלישי: Mockito למוקינג

לבדיקת קוד שיש לו תלות מורכבת, ייתכן שתשתמש ב-Mockito ליצירת אובייקטים מדומים. ראשית, הוסף את Mockito ל`pubspec.yaml` שלך:

```yaml
dev_dependencies:
  mockito: ^5.0.0
```

נניח שיש לך מחלקה `UserRepository` שמשיגה נתוני משתמש, וברצונך לבדוק שירות `UserService` שתלוי ב-`UserRepository` ללא היכנסות למסד נתונים אמיתי:

```dart
import 'package:mockito/mockito.dart';
import 'package:test/test.dart';
import 'package:your_project/user_repository.dart';
import 'package:your_project/user_service.dart';

// יצירת כיתת Mock באמצעות Mockito
class MockUserRepository extends Mock implements UserRepository {}

void main() {
  group('בדיקות UserService', () {
    test('משיג משתמש בהצלחה', () {
      // יצירת מופע מדומה
      final mockUserRepository = MockUserRepository();
      final userService = UserService(mockUserRepository);

      // הגדרת התנהגות מדומה
      when(mockUserRepository.fetchUser(1)).thenReturn(User(id: 1, name: 'Test User'));

      // טענה שהמתודה המדומה נקראה עם הארגומנטים הצפויים
      expect(userService.getUserName(1), 'Test User');
      verify(mockUserRepository.fetchUser(1)).called(1);
    });
  });
}
```

הרצת הטסט מאשרת ש-`UserService` מתקשרת נכון עם `UserRepository`, באמצעות מוקינג כדי לדמות את האינטראקציות האמיתיות בדרך שליטה.
