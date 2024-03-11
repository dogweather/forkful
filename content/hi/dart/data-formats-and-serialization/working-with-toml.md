---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:34.025173-07:00
description: "TOML, \u092F\u093E \u091F\u0949\u092E \u0915\u0940 \u0938\u094D\u092A\
  \u0937\u094D\u091F, \u0928\u094D\u092F\u0942\u0928\u0924\u092E \u092D\u093E\u0937\
  \u093E, \u090F\u0915 \u0915\u0949\u0928\u094D\u092B\u093C\u093F\u0917\u0930\u0947\
  \u0936\u0928 \u092B\u093C\u093E\u0907\u0932 \u092B\u093C\u0949\u0930\u094D\u092E\
  \u0947\u091F \u0939\u0948 \u091C\u094B \u0907\u0938\u0915\u0940 \u0938\u093E\u092B\
  \u093C \u0938\u092E\u093E\u0928\u094D\u0924\u0930\u0924\u093E \u0915\u0947 \u0915\
  \u093E\u0930\u0923 \u092A\u0922\u093C\u0928\u0947 \u092E\u0947\u0902 \u0906\u0938\
  \u093E\u0928 \u0939\u094B\u0924\u0940 \u0939\u0948\u0964 \u092A\u094D\u0930\u094B\
  \u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0907\u0938\u0915\u093E \u0909\
  \u092A\u092F\u094B\u0917\u2026"
lastmod: '2024-03-11T00:14:25.734362-06:00'
model: gpt-4-0125-preview
summary: "TOML, \u092F\u093E \u091F\u0949\u092E \u0915\u0940 \u0938\u094D\u092A\u0937\
  \u094D\u091F, \u0928\u094D\u092F\u0942\u0928\u0924\u092E \u092D\u093E\u0937\u093E\
  , \u090F\u0915 \u0915\u0949\u0928\u094D\u092B\u093C\u093F\u0917\u0930\u0947\u0936\
  \u0928 \u092B\u093C\u093E\u0907\u0932 \u092B\u093C\u0949\u0930\u094D\u092E\u0947\
  \u091F \u0939\u0948 \u091C\u094B \u0907\u0938\u0915\u0940 \u0938\u093E\u092B\u093C\
  \ \u0938\u092E\u093E\u0928\u094D\u0924\u0930\u0924\u093E \u0915\u0947 \u0915\u093E\
  \u0930\u0923 \u092A\u0922\u093C\u0928\u0947 \u092E\u0947\u0902 \u0906\u0938\u093E\
  \u0928 \u0939\u094B\u0924\u0940 \u0939\u0948\u0964 \u092A\u094D\u0930\u094B\u0917\
  \u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0907\u0938\u0915\u093E \u0909\u092A\
  \u092F\u094B\u0917\u2026"
title: "TOML \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\
  \u093E"
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
