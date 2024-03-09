---
title:                "עובדים עם TOML"
date:                  2024-03-08T21:58:08.867153-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
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
