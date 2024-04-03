---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:58:08.867153-07:00
description: "\u05D0\u05D9\u05DA \u05DC\u05E2\u05E9\u05D5\u05EA: Dart \u05D0\u05D9\
  \u05E0\u05D5 \u05DB\u05D5\u05DC\u05DC \u05EA\u05DE\u05D9\u05DB\u05D4 \u05DE\u05D5\
  \u05D1\u05E0\u05D9\u05EA \u05DC-TOML, \u05D0\u05DA \u05E0\u05D9\u05EA\u05DF \u05DC\
  \u05E2\u05D1\u05D5\u05D3 \u05E2\u05DD \u05E7\u05D1\u05E6\u05D9 TOML \u05D1\u05D0\
  \u05DE\u05E6\u05E2\u05D5\u05EA \u05D7\u05D1\u05D9\u05DC\u05D5\u05EA \u05E6\u05D3\
  \ \u05E9\u05DC\u05D9\u05E9\u05D9 \u05DB\u05DE\u05D5 `toml`. \u05E8\u05D0\u05E9\u05D9\
  \u05EA, \u05D4\u05D5\u05E1\u05D9\u05E4\u05D5 \u05D0\u05EA `toml` \u05DC`pubspec.yaml`\
  \ \u05E9\u05DC\u05DB\u05DD."
lastmod: '2024-03-13T22:44:38.886073-06:00'
model: gpt-4-0125-preview
summary: "Dart \u05D0\u05D9\u05E0\u05D5 \u05DB\u05D5\u05DC\u05DC \u05EA\u05DE\u05D9\
  \u05DB\u05D4 \u05DE\u05D5\u05D1\u05E0\u05D9\u05EA \u05DC-TOML, \u05D0\u05DA \u05E0\
  \u05D9\u05EA\u05DF \u05DC\u05E2\u05D1\u05D5\u05D3 \u05E2\u05DD \u05E7\u05D1\u05E6\
  \u05D9 TOML \u05D1\u05D0\u05DE\u05E6\u05E2\u05D5\u05EA \u05D7\u05D1\u05D9\u05DC\u05D5\
  \u05EA \u05E6\u05D3 \u05E9\u05DC\u05D9\u05E9\u05D9 \u05DB\u05DE\u05D5 `toml`."
title: "\u05E2\u05D5\u05D1\u05D3\u05D9\u05DD \u05E2\u05DD TOML"
weight: 39
---

## איך לעשות:
Dart אינו כולל תמיכה מובנית ל-TOML, אך ניתן לעבוד עם קבצי TOML באמצעות חבילות צד שלישי כמו `toml`. ראשית, הוסיפו את `toml` ל`pubspec.yaml` שלכם:

```yaml
dependencies:
  toml: ^0.10.0
```

### קריאת TOML
כדי לקרוא קובץ TOML, בואו נניח שיש לכם קובץ תצורה פשוט `config.toml`:

```toml
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
```

ניתן לנתח קובץ TOML זה ב-Dart כך:

```dart
import 'dart:io';
import 'package:toml/toml.dart';

void main() async {
  var content = await File('config.toml').readAsString();
  var doc = TomlDocument.parse(content);
  var data = doc.toMap();

  print(data['database']); // מדפיס את חלק ה'database'
}
```

דבר זה מדפיס:

```dart
{server: 192.168.1.1, ports: [8001, 8001, 8002], connection_max: 5000, enabled: true}
```

### כתיבת TOML
כדי ליצור תוכן TOML, השתמשו ב-`TomlBuilder` המסופק על ידי חבילת ה`toml`:

```dart
import 'package:toml/toml.dart';

void main() {
  final builder = TomlBuilder();

  builder.table('database')
    ..set('server', '192.168.1.1')
    ..set('ports', [8001, 8001, 8002])
    ..set('connection_max', 5000)
    ..set('enabled', true);

  var tomlString = builder.build().toString();
  print(tomlString);
}
```

זה יגרום וידפיס מחרוזת נציגה של התוכן TOML, דומה מאוד לקובץ תצורה `config.toml` שלנו:

```toml
[database]
server = "192.168.1.1"
ports = [8001, 8001, 8002]
connection_max = 5000
enabled = true
```

דוגמאות אלו מראות כיצד לקרוא מקבצי TOML ולכתוב אליהם, הופכות את העבודה עם נתוני תצורה ביישומי Dart שלכם לפשוטה.
