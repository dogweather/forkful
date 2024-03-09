---
title:                "TOML के साथ काम करना"
date:                  2024-03-08T21:57:34.025173-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

TOML, या टॉम की स्पष्ट, न्यूनतम भाषा, एक कॉन्फ़िगरेशन फ़ाइल फ़ॉर्मेट है जो इसकी साफ़ समान्तरता के कारण पढ़ने में आसान होती है। प्रोग्रामर्स इसका उपयोग सॉफ्टवेयर एप्लिकेशन्स को कॉन्फ़िगर करने के लिए करते हैं क्योंकि यह पार्स करने में सरल होता है और न्यूनतम भ्रम या त्रुटियों को उत्पन्न करता है।

## कैसे:

Dart में TOML के लिए बनाई गई सुविधा शामिल नहीं है, परंतु आप `toml` जैसे तृतीय-पक्ष पैकेजेस का उपयोग करके TOML फ़ाइलों के साथ काम कर सकते हैं। सबसे पहले, अपनी `pubspec.yaml` में `toml` जोड़ें:

```yaml
dependencies:
  toml: ^0.10.0
```

### TOML पढ़ना

एक TOML फ़ाइल पढ़ने के लिए, मान लेते हैं आपके पास एक सरल कॉन्फ़िगरेशन फ़ाइल `config.toml` है:

```toml
[database]
server = "192.168.1.1"
ports = [ 8001, 8001, 8002 ]
connection_max = 5000
enabled = true
```

आप Dart में इस TOML फ़ाइल को निम्न प्रकार से पार्स कर सकते हैं:

```dart
import 'dart:io';
import 'package:toml/toml.dart';

void main() async {
  var content = await File('config.toml').readAsString();
  var doc = TomlDocument.parse(content);
  var data = doc.toMap();

  print(data['database']); // 'database' खंड को प्रिंट करें
}
```

यह प्रिंट करेगा:

```dart
{server: 192.168.1.1, ports: [8001, 8001, 8002], connection_max: 5000, enabled: true}
```

### TOML लिखना

TOML सामग्री बनाने के लिए, `toml` पैकेज द्वारा प्रदान किया गया `TomlBuilder` का इस्तेमाल करें:

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

यह हमारी `config.toml` फ़ाइल के बहुत समान एक TOML सामग्री का स्ट्रिंग रिप्रेज़ेन्टेशन उत्पन्न और प्रिंट करेगा:

```toml
[database]
server = "192.168.1.1"
ports = [8001, 8001, 8002]
connection_max = 5000
enabled = true
```

ये उदाहरण दिखाते हैं कि कैसे TOML फ़ाइलों से पढ़ना और उसमें लिखना होता है, जिससे आपके Dart एप्लिकेशन में कॉन्फ़िगरेशन डेटा के साथ काम करना सरल हो जाता है।
