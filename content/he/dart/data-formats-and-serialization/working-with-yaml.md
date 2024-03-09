---
title:                "עובדים עם YAML"
date:                  2024-03-08T21:57:59.694348-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## מה ולמה?

YAML, שהמשמעות של הראשי תיבות היא "YAML Ain't Markup Language", היא פורמט סידורי נתונים קריא לאדם. מתכנתים משתמשים בה עבור קבצי קונפיגורציה, החלפת נתונים, וביישומים שבהם נדרש לאחסן או לשדר נתונים בפורמט שקל להבין.

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
