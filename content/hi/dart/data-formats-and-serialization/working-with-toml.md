---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:34.025173-07:00
description: "\u0915\u0948\u0938\u0947: Dart \u092E\u0947\u0902 TOML \u0915\u0947\
  \ \u0932\u093F\u090F \u092C\u0928\u093E\u0908 \u0917\u0908 \u0938\u0941\u0935\u093F\
  \u0927\u093E \u0936\u093E\u092E\u093F\u0932 \u0928\u0939\u0940\u0902 \u0939\u0948\
  , \u092A\u0930\u0902\u0924\u0941 \u0906\u092A `toml` \u091C\u0948\u0938\u0947 \u0924\
  \u0943\u0924\u0940\u092F-\u092A\u0915\u094D\u0937 \u092A\u0948\u0915\u0947\u091C\
  \u0947\u0938 \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\u0947\
  \ TOML \u092B\u093C\u093E\u0907\u0932\u094B\u0902 \u0915\u0947 \u0938\u093E\u0925\
  \ \u0915\u093E\u092E \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\u0964\
  \ \u0938\u092C\u0938\u0947\u2026"
lastmod: '2024-03-13T22:44:51.856179-06:00'
model: gpt-4-0125-preview
summary: "Dart \u092E\u0947\u0902 TOML \u0915\u0947 \u0932\u093F\u090F \u092C\u0928\
  \u093E\u0908 \u0917\u0908 \u0938\u0941\u0935\u093F\u0927\u093E \u0936\u093E\u092E\
  \u093F\u0932 \u0928\u0939\u0940\u0902 \u0939\u0948, \u092A\u0930\u0902\u0924\u0941\
  \ \u0906\u092A `toml` \u091C\u0948\u0938\u0947 \u0924\u0943\u0924\u0940\u092F-\u092A\
  \u0915\u094D\u0937 \u092A\u0948\u0915\u0947\u091C\u0947\u0938 \u0915\u093E \u0909\
  \u092A\u092F\u094B\u0917 \u0915\u0930\u0915\u0947 TOML \u092B\u093C\u093E\u0907\u0932\
  \u094B\u0902 \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930 \u0938\
  \u0915\u0924\u0947 \u0939\u0948\u0902\u0964 \u0938\u092C\u0938\u0947 \u092A\u0939\
  \u0932\u0947, \u0905\u092A\u0928\u0940 `pubspec.yaml` \u092E\u0947\u0902 `toml`\
  \ \u091C\u094B\u0921\u093C\u0947\u0902."
title: "TOML \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\
  \u093E"
weight: 39
---

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
