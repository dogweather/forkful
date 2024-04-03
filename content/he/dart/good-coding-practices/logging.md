---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:29.818406-07:00
description: "\u05DC\u05D5\u05D2\u05D9\u05E0\u05D2 \u05D1-Dart \u05DE\u05EA\u05D9\u05D9\
  \u05D7\u05E1 \u05DC\u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05DC \u05E8\u05D9\u05E9\
  \u05D5\u05DD \u05E8\u05DE\u05D5\u05EA \u05E9\u05D5\u05E0\u05D5\u05EA \u05E9\u05DC\
  \ \u05DE\u05D9\u05D3\u05E2 \u05D1\u05DE\u05D4\u05DC\u05DA \u05D1\u05D9\u05E6\u05D5\
  \u05E2 \u05D4\u05EA\u05D5\u05DB\u05E0\u05D9\u05EA. \u05DE\u05EA\u05DB\u05E0\u05EA\
  \u05D9\u05DD \u05E2\u05D5\u05E9\u05D9\u05DD \u05D6\u05D0\u05EA \u05DB\u05D3\u05D9\
  \ \u05DC\u05E2\u05E7\u05D5\u05D1 \u05D0\u05D7\u05E8 \u05D4\u05EA\u05E0\u05D4\u05D2\
  \u05D5\u05EA \u05D4\u05EA\u05D5\u05DB\u05E0\u05D4, \u05DC\u05D0\u05D1\u05D7\u05DF\
  \ \u05D1\u05E2\u05D9\u05D5\u05EA \u05D5\u05DC\u05E0\u05EA\u05D7 \u05D1\u05D9\u05E6\
  \u05D5\u05E2\u05D9\u05DD, \u05DE\u05D4\u2026"
lastmod: '2024-03-13T22:44:38.857273-06:00'
model: gpt-4-0125-preview
summary: "\u05DC\u05D5\u05D2\u05D9\u05E0\u05D2 \u05D1-Dart \u05DE\u05EA\u05D9\u05D9\
  \u05D7\u05E1 \u05DC\u05EA\u05D4\u05DC\u05D9\u05DA \u05E9\u05DC \u05E8\u05D9\u05E9\
  \u05D5\u05DD \u05E8\u05DE\u05D5\u05EA \u05E9\u05D5\u05E0\u05D5\u05EA \u05E9\u05DC\
  \ \u05DE\u05D9\u05D3\u05E2 \u05D1\u05DE\u05D4\u05DC\u05DA \u05D1\u05D9\u05E6\u05D5\
  \u05E2 \u05D4\u05EA\u05D5\u05DB\u05E0\u05D9\u05EA."
title: "\u05E8\u05D9\u05E9\u05D5\u05DD"
weight: 17
---

## איך לעשות:
Dart כולל מנגנון לוגינג פשוט דרך הספרייה `dart:developer`. לצרכים מתקדמים יותר של לוגינג, מתכנתים לעיתים קרובות נעזרים בספריות צד שלישי כמו `logger` ו-`log4dart`.

### שימוש ב-`dart:developer`
מתאים ללוגינג בסיסי, בעיקר במהלך הפיתוח:

```dart
import 'dart:developer';

void main() {
  log('This is a debug log message.');
}
```

פלט:
```
This is a debug log message.
```

### שימוש בחבילת `logger`
לפתרון יותר מקיף, חבילת `logger` מציעה רמות שונות של לוגינג (למשל, מידע, אזהרה, שגיאה) וניתן לעצב אותה בצורה קריאה יותר.

ראשית, הוסף את תלות `logger` בקובץ `pubspec.yaml` שלך:

```yaml
dependencies:
  logger: ^1.0.0
```

לאחר מכן, השתמש בה כך:

```dart
import 'package:logger/logger.dart';

var logger = Logger();

void main() {
  logger.d("This is a debug message");
  logger.w("This is a warning message");
  logger.e("This is an error message");
}
```

דוגמה לפלט עשויה להיראות כך, כאשר כל סוג הודעה מעוצב אחרת לזיהוי קל:

```
💬 This is a debug message
⚠️ This is a warning message
❗️ This is an error message
```

### שימוש בחבילת `log4dart`
לאפליקציות הדורשות לוגינג מבוסס תצורה (דומה ל-Log4j), `log4dart` מציעה גישה מוכרת. זה במיוחד שימושי לאפליקציות בקנה מידה גדול.

ודא שכללת את `log4dart` ב-`pubspec.yaml` שלך:

```yaml
dependencies:
  log4dart: ^2.0.0
```

דוגמה לשימוש פשוטה:

```dart
import 'package:log4dart/log4dart.dart';

void main() {
  final logger = LoggerFactory.getLogger("MyApp");
  logger.debug("Debugging MyApp");
  logger.info("Informational message");
}
```

פלט:

```
DEBUG: Debugging MyApp
INFO: Informational message
```

כל אחת מהשיטות הללו מספקת רמה שונה של גמישות ומורכבות, החל מהודעות ניפוי שגיאות פשוטות ועד ללוגינג מקיף ומתצורה המתאים לצרכים של אפליקציות מורכבות.
