---
title:                "הדפסת פלט לניפוי שגיאות"
date:                  2024-03-08T21:56:21.972261-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

הדפסת פלט לניפוי באגים ב-Dart היא על תצוגת מידע לקונסולה בזמן ריצה, מה שמאפשר למפתחים לעקוב אחר זרימת הביצועים, לחקור את מצב המשתנים, או לזהות את מקור השגיאות. תכנתנים בדרך כלל משתמשים בזה לצורכי ניפוי באגים ולאימות שהקוד שלהם פועל כפי שציפו, מה שמקל על תהליך הפיתוח והופך אותו ליעיל יותר.

## איך לעשות:

ב-Dart, אפשר להדפיס פלט לניפוי באגים באמצעות הפונקציה `print()`. הנה איך להדפיס הודעות פשוטות וערכי משתנים:

```dart
void main() {
  String greeting = "שלום, Dart!";
  print(greeting); // מדפיס: שלום, Dart!

  int number = 42;
  print('המספר הוא $number.'); // מדפיס: המספר הוא 42.
}
```

לנתונים מובנים, כמו רשימות או אובייקטים, ייתכן ששיטת ה-`toString()` של Dart לא תספק מספיק פרטים. במקרים כאלה, אפשר להשתמש בפונקציה `jsonEncode` מספריית ה-`dart:convert` של Dart כדי להמיר את הנתונים למחרוזת JSON לפלט קריא יותר:

```dart
import 'dart:convert';

void main() {
  var user = {
    'name': 'John Doe',
    'age': 30,
    'emails': ['john.doe@example.com', 'john@example.com'],
  };

  print(jsonEncode(user));
  // מדפיס: {"name":"John Doe","age":30,"emails":["john.doe@example.com","john@example.com"]}
}
```

כאשר נדרשות יכולות ניפוי באגים מתקדמות יותר, כמו תיעוד עם רמות חשיבות שונות (מידע, אזהרה, שגיאה), אפשר להשתמש בספריות של גורמים שלישיים כמו `logger`. הנה איך להשתמש בזה:

1. הוסף את `logger` ל-`pubspec.yaml` שלך:

```yaml
dependencies:
  logger: ^1.0.0
```

2. השתמש ב-`logger` בקוד Dart שלך:

```dart
import 'package:logger/logger.dart';

var logger = Logger();

void main() {
  logger.d("זו הודעת ניפוי באגים");
  logger.w("זו הודעת אזהרה");
  logger.e("זו הודעת שגיאה");
}
```

הפלט יהיה מודיע יותר, מציג את רמת ההודעה ואת ההודעה עצמה, מה שהופך את ההבחנה בין סוגי הודעות התיעוד לקלה יותר.
