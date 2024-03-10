---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:58:08.867153-07:00
description: "TOML, \u05D0\u05D5 \u05E9\u05E4\u05EA \u05D4\u05EA\u05E6\u05D5\u05E8\
  \u05D4 \u05D4\u05DE\u05D9\u05E0\u05D9\u05DE\u05DC\u05D9\u05EA \u05D5\u05D4\u05D1\
  \u05E8\u05D5\u05E8\u05D4 \u05E9\u05DC \u05D8\u05D5\u05DD, \u05D4\u05D9\u05D0 \u05E4\
  \u05D5\u05E8\u05DE\u05D8 \u05E7\u05D5\u05D1\u05E5 \u05EA\u05E6\u05D5\u05E8\u05D4\
  \ \u05E9\u05E7\u05DC \u05DC\u05E7\u05E8\u05D5\u05D0 \u05D1\u05D6\u05DB\u05D5\u05EA\
  \ \u05D4\u05E1\u05DE\u05E0\u05D8\u05D9\u05E7\u05D4 \u05D4\u05D1\u05E8\u05D5\u05E8\
  \u05D4 \u05E9\u05DC\u05D5. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\
  \u05EA\u05DE\u05E9\u05D9\u05DD \u05D1\u05D5 \u05DC\u05EA\u05E6\u05D5\u05E8\u05EA\
  \ \u05D9\u05D9\u05E9\u05D5\u05DE\u05D9 \u05EA\u05D5\u05DB\u05E0\u05D4 \u05DE\u05DB\
  \u05D9\u05D5\u05D5\u05DF\u2026"
lastmod: '2024-03-09T21:06:03.652311-07:00'
model: gpt-4-0125-preview
summary: "TOML, \u05D0\u05D5 \u05E9\u05E4\u05EA \u05D4\u05EA\u05E6\u05D5\u05E8\u05D4\
  \ \u05D4\u05DE\u05D9\u05E0\u05D9\u05DE\u05DC\u05D9\u05EA \u05D5\u05D4\u05D1\u05E8\
  \u05D5\u05E8\u05D4 \u05E9\u05DC \u05D8\u05D5\u05DD, \u05D4\u05D9\u05D0 \u05E4\u05D5\
  \u05E8\u05DE\u05D8 \u05E7\u05D5\u05D1\u05E5 \u05EA\u05E6\u05D5\u05E8\u05D4 \u05E9\
  \u05E7\u05DC \u05DC\u05E7\u05E8\u05D5\u05D0 \u05D1\u05D6\u05DB\u05D5\u05EA \u05D4\
  \u05E1\u05DE\u05E0\u05D8\u05D9\u05E7\u05D4 \u05D4\u05D1\u05E8\u05D5\u05E8\u05D4\
  \ \u05E9\u05DC\u05D5. \u05DE\u05EA\u05DB\u05E0\u05EA\u05D9\u05DD \u05DE\u05E9\u05EA\
  \u05DE\u05E9\u05D9\u05DD \u05D1\u05D5 \u05DC\u05EA\u05E6\u05D5\u05E8\u05EA \u05D9\
  \u05D9\u05E9\u05D5\u05DE\u05D9 \u05EA\u05D5\u05DB\u05E0\u05D4 \u05DE\u05DB\u05D9\
  \u05D5\u05D5\u05DF\u2026"
title: "\u05E2\u05D5\u05D1\u05D3\u05D9\u05DD \u05E2\u05DD TOML"
---

{{< edit_this_page >}}

## מה ולמה?

TOML, או שפת התצורה המינימלית והברורה של טום, היא פורמט קובץ תצורה שקל לקרוא בזכות הסמנטיקה הברורה שלו. מתכנתים משתמשים בו לתצורת יישומי תוכנה מכיוון שהפענוח שלו פשוט והוא גורם למינימום תקלות או בלבול.

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
