---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:10.205219-07:00
description: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05D0\u05E8\u05D2\u05D5\u05DE\u05E0\u05D8\
  \u05D9\u05DD \u05DE\u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4\
  \ \u05D1-Dart \u05DE\u05D0\u05E4\u05E9\u05E8\u05EA \u05DC\u05DE\u05EA\u05DB\u05E0\
  \u05EA\u05D9\u05DD \u05DC\u05D4\u05D6\u05D9\u05DF \u05E0\u05EA\u05D5\u05E0\u05D9\
  \u05DD \u05D9\u05E9\u05D9\u05E8\u05D5\u05EA \u05DC\u05E7\u05D5\u05E0\u05E1\u05D5\
  \u05DC \u05D1\u05E2\u05EA \u05D4\u05E8\u05E6\u05EA \u05EA\u05D5\u05DB\u05E0\u05D9\
  \u05EA Dart, \u05D5\u05D1\u05DB\u05DA \u05DC\u05E9\u05E4\u05E8 \u05D0\u05EA \u05D4\
  \u05D0\u05D9\u05E0\u05D8\u05E8\u05D0\u05E7\u05D8\u05D9\u05D1\u05D9\u05D5\u05EA \u05D5\
  \u05D4\u05D2\u05DE\u05D9\u05E9\u05D5\u05EA \u05E9\u05DC\u05D4 \u05E2\u05D1\u05D5\
  \u05E8\u2026"
lastmod: '2024-03-11T00:14:12.301669-06:00'
model: gpt-4-0125-preview
summary: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05D0\u05E8\u05D2\u05D5\u05DE\u05E0\u05D8\
  \u05D9\u05DD \u05DE\u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4\
  \ \u05D1-Dart \u05DE\u05D0\u05E4\u05E9\u05E8\u05EA \u05DC\u05DE\u05EA\u05DB\u05E0\
  \u05EA\u05D9\u05DD \u05DC\u05D4\u05D6\u05D9\u05DF \u05E0\u05EA\u05D5\u05E0\u05D9\
  \u05DD \u05D9\u05E9\u05D9\u05E8\u05D5\u05EA \u05DC\u05E7\u05D5\u05E0\u05E1\u05D5\
  \u05DC \u05D1\u05E2\u05EA \u05D4\u05E8\u05E6\u05EA \u05EA\u05D5\u05DB\u05E0\u05D9\
  \u05EA Dart, \u05D5\u05D1\u05DB\u05DA \u05DC\u05E9\u05E4\u05E8 \u05D0\u05EA \u05D4\
  \u05D0\u05D9\u05E0\u05D8\u05E8\u05D0\u05E7\u05D8\u05D9\u05D1\u05D9\u05D5\u05EA \u05D5\
  \u05D4\u05D2\u05DE\u05D9\u05E9\u05D5\u05EA \u05E9\u05DC\u05D4 \u05E2\u05D1\u05D5\
  \u05E8\u2026"
title: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05D0\u05E8\u05D2\u05D5\u05DE\u05E0\u05D8\u05D9\
  \u05DD \u05DE\u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4"
---

{{< edit_this_page >}}

## מה ולמה?

קריאת ארגומנטים משורת הפקודה ב-Dart מאפשרת למתכנתים להזין נתונים ישירות לקונסול בעת הרצת תוכנית Dart, ובכך לשפר את האינטראקטיביות והגמישות שלה עבור שימושים שונים, כולל סקריפטים לאוטומציה, כלים שורת פקודה (CLI), או עיבוד אצווה. תכונה זו חיונית ליצירת יישומי שורת פקודה מותאמים אישית ונוחים למשתמש.

## איך לעשות זאת:

Dart מספקת גישה ישירה לקריאת ארגומנטים משורת הפקודה דרך `List<String> args` במתודת ה-main. להלן דוגמה פשוטה המדגימה איך לקרוא ולהשתמש בארגומנטים משורת הפקודה.

```dart
// main.dart
void main(List<String> args) {
  print('ארגומנטים משורת הפקודה:');
  for (var i = 0; i < args.length; i++) {
    print('${i + 1}: ${args[i]}');
  }
}
```

כדי להפעיל תוכנית Dart זו ולהעביר ארגומנטים משורת הפקודה, השתמשו ב-Dart CLI כך:

```shell
dart run main.dart Hello World!
```

הפלט הצפוי:

```
ארגומנטים משורת הפקודה:
1: Hello
2: World!
```

### שימוש בספרייה פופולרית של צד שלישי: `args`

למרות שהיכולות המובנות של Dart לטיפול בארגומנטים משורת הפקודה חזקות למגוון יישומים, החבילה `args` מספקת דרך מדודה יותר להגדיר ולנתח ארגומנטים משורת הפקודה עבור צרכים מורכבים יותר.

ראשית, הוסיפו את חבילת `args` ל-`pubspec.yaml` שלכם:

```yaml
dependencies:
  args: ^2.0.0
```

לאחר מכן, השתמשו בה בתוכנית שלכם כך:

```dart
// שימוש בחבילת 'args'
import 'package:args/args.dart';

void main(List<String> arguments) {
  final parser = ArgParser()..addOption('name', abbr: 'n');
  final argResults = parser.parse(arguments);

  if (argResults.wasParsed('name')) {
    print('שלום, ${argResults['name']}!');
  } else {
    print('לא סופק שם.');
  }
}
```

הרצו את התוכנית עם ארגומנט מסווג:

```shell
dart run main.dart --name=John
```

הפלט הצפוי:

```
שלום, John!
```

הצגה זו המקדמת את נושא ניתוח ארגומנטים משורת הפקודה, באופן טבעי ובאמצעות ספריית ה-`args`, מציגה כיצד Dart יכולה לטפל בקלטי משתמש ישירות מהקונסול, פותחת דרך ליצירת יישומי CLI אינטראקטיביים ודינמיים יותר.
