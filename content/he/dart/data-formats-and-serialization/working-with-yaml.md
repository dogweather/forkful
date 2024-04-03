---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:59.694348-07:00
description: "\u05D0\u05D9\u05DA \u05DC: \u05D1-Dart, \u05E2\u05D1\u05D5\u05D3\u05D4\
  \ \u05E2\u05DD YAML \u05DE\u05E2\u05E8\u05D1\u05EA \u05DC\u05E8\u05D5\u05D1 \u05E9\
  \u05D9\u05DE\u05D5\u05E9 \u05D1\u05E1\u05E4\u05E8\u05D9\u05D4 \u05D7\u05D9\u05E6\
  \u05D5\u05E0\u05D9\u05EA \u05DB\u05D9\u05D5\u05D5\u05DF \u05E9\u05D4\u05E9\u05E4\
  \u05D4 \u05D0\u05D9\u05E0\u05D4 \u05DB\u05D5\u05DC\u05DC\u05EA \u05D9\u05DB\u05D5\
  \u05DC\u05D5\u05EA \u05E0\u05D9\u05EA\u05D5\u05D7 YAML \u05DE\u05D5\u05D1\u05E0\u05D5\
  \u05EA. \u05D1\u05D7\u05D9\u05E8\u05D4 \u05E4\u05D5\u05E4\u05D5\u05DC\u05E8\u05D9\
  \u05EA \u05D4\u05D9\u05D0 \u05D4\u05D7\u05D1\u05D9\u05DC\u05D4 `yaml`. \u05DC\u05D4\
  \u05EA\u05D7\u05D9\u05DC,\u2026"
lastmod: '2024-03-13T22:44:38.881286-06:00'
model: gpt-4-0125-preview
summary: "\u05D1-Dart, \u05E2\u05D1\u05D5\u05D3\u05D4 \u05E2\u05DD YAML \u05DE\u05E2\
  \u05E8\u05D1\u05EA \u05DC\u05E8\u05D5\u05D1 \u05E9\u05D9\u05DE\u05D5\u05E9 \u05D1\
  \u05E1\u05E4\u05E8\u05D9\u05D4 \u05D7\u05D9\u05E6\u05D5\u05E0\u05D9\u05EA \u05DB\
  \u05D9\u05D5\u05D5\u05DF \u05E9\u05D4\u05E9\u05E4\u05D4 \u05D0\u05D9\u05E0\u05D4\
  \ \u05DB\u05D5\u05DC\u05DC\u05EA \u05D9\u05DB\u05D5\u05DC\u05D5\u05EA \u05E0\u05D9\
  \u05EA\u05D5\u05D7 YAML \u05DE\u05D5\u05D1\u05E0\u05D5\u05EA."
title: "\u05E2\u05D5\u05D1\u05D3\u05D9\u05DD \u05E2\u05DD YAML"
weight: 41
---

## איך ל:
ב-Dart, עבודה עם YAML מערבת לרוב שימוש בספריה חיצונית כיוון שהשפה אינה כוללת יכולות ניתוח YAML מובנות. בחירה פופולרית היא החבילה `yaml`. להתחיל, תצטרך להוסיף את החבילה הזו ל-`pubspec.yaml` שלך:

```yaml
dependencies:
  yaml: ^3.1.0
```

זכור להריץ `pub get` כדי להוריד את החבילה.

### קריאת YAML
כדי לקרוא קובץ YAML, ראשית יבוא החבילה `yaml` ואז השתמש בפונקציה `loadYaml`:

```dart
import 'package:yaml/yaml.dart';
import 'dart:io';

void main() {
  final file = File('config.yaml').readAsStringSync();
  final yamlMap = loadYaml(file);

  print(yamlMap['name']); // פלט: John Doe
}

```

בהנחה שהקובץ `config.yaml` שלך נראה כך:

```yaml
name: John Doe
age: 30
```

### כתיבת YAML
אף על פי שחבילת `yaml` נהדרת לניתוח, היא אינה תומכת בכתיבת YAML. לשם כך, ייתכן שתצטרך להמיר את הנתונים שלך ל-YAML באופן ידני או להשתמש בחבילה אחרת אם קיימת. או, באופן פשוט יותר, לנהל את התמרות הנתונים שלך ולהוציא אותם כמחרוזות התואמות תחביר YAML:

```dart
Map<String, dynamic> data = {
  'name': 'Jane Doe',
  'age': 29,
};

String toYamlString(Map<String, dynamic> map) {
  String yaml = '';
  map.forEach((key, value) {
    yaml += '$key: $value\n';
  });
  return yaml;
}

void main() {
  print(toYamlString(data)); // פלט: name: Jane Doe
                             //       age: 29
}
```

זהו גישה פשטנית ועשוי לא להתאים למבני נתונים מורכבים או תכונות מיוחדות של YAML. לצרכים מתוחכמים יותר, ייתכן שיהיה צורך לחפש או לתרום לחבילת Dart יותר מקיפה.
