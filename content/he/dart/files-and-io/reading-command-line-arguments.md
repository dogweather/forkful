---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:10.205219-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA \u05D6\u05D0\u05EA\
  : Dart \u05DE\u05E1\u05E4\u05E7\u05EA \u05D2\u05D9\u05E9\u05D4 \u05D9\u05E9\u05D9\
  \u05E8\u05D4 \u05DC\u05E7\u05E8\u05D9\u05D0\u05EA \u05D0\u05E8\u05D2\u05D5\u05DE\
  \u05E0\u05D8\u05D9\u05DD \u05DE\u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\
  \u05D3\u05D4 \u05D3\u05E8\u05DA `List<String> args` \u05D1\u05DE\u05EA\u05D5\u05D3\
  \u05EA \u05D4-main. \u05DC\u05D4\u05DC\u05DF \u05D3\u05D5\u05D2\u05DE\u05D4 \u05E4\
  \u05E9\u05D5\u05D8\u05D4 \u05D4\u05DE\u05D3\u05D2\u05D9\u05DE\u05D4 \u05D0\u05D9\
  \u05DA \u05DC\u05E7\u05E8\u05D5\u05D0 \u05D5\u05DC\u05D4\u05E9\u05EA\u05DE\u05E9\
  \u2026"
lastmod: '2024-03-13T22:44:38.872831-06:00'
model: gpt-4-0125-preview
summary: "Dart \u05DE\u05E1\u05E4\u05E7\u05EA \u05D2\u05D9\u05E9\u05D4 \u05D9\u05E9\
  \u05D9\u05E8\u05D4 \u05DC\u05E7\u05E8\u05D9\u05D0\u05EA \u05D0\u05E8\u05D2\u05D5\
  \u05DE\u05E0\u05D8\u05D9\u05DD \u05DE\u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\
  \u05D5\u05D3\u05D4 \u05D3\u05E8\u05DA `List<String> args` \u05D1\u05DE\u05EA\u05D5\
  \u05D3\u05EA \u05D4-main."
title: "\u05E7\u05E8\u05D9\u05D0\u05EA \u05D0\u05E8\u05D2\u05D5\u05DE\u05E0\u05D8\u05D9\
  \u05DD \u05DE\u05E9\u05D5\u05E8\u05EA \u05D4\u05E4\u05E7\u05D5\u05D3\u05D4"
weight: 23
---

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
