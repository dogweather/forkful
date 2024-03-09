---
title:                "YAML के साथ काम करना"
date:                  2024-03-08T21:58:12.562261-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

YAML, जो कि YAML Ain't Markup Language का संक्षिप्त रूप है, एक मानव-पठनीय डेटा सीरियलाइजेशन प्रारूप है। प्रोग्रामर इसे कॉन्फ़िगरेशन फ़ाइलों, डेटा एक्सचेंज, और उन अनुप्रयोगों में इस्तेमाल करते हैं जहां डेटा को एक आसानी से समझे जाने वाले प्रारूप में संग्रहीत या प्रेषित किया जाने की आवश्यकता होती है।

## कैसे:

Dart में, YAML के साथ काम करना आमतौर पर तृतीय-पक्ष पुस्तकालय का उपयोग करके होता है क्योंकि भाषा में बिल्ट-इन YAML पार्सिंग क्षमताएं शामिल नहीं होती हैं। एक लोकप्रिय विकल्प `yaml` पैकेज है। शुरू करने के लिए, आपको इस पैकेज को अपने `pubspec.yaml` में जोड़ने की आवश्यकता होगी:

```yaml
dependencies:
  yaml: ^3.1.0
```

पैकेज को प्राप्त करने के लिए `pub get` चलाना न भूलें।

### YAML पढ़ना

एक YAML फाइल पढ़ने के लिए, पहले `yaml` पैकेज को इम्पोर्ट करें और फिर `loadYaml` फंक्शन का उपयोग करें:

```dart
import 'package:yaml/yaml.dart';
import 'dart:io';

void main() {
  final file = File('config.yaml').readAsStringSync();
  final yamlMap = loadYaml(file);

  print(yamlMap['name']); // आउटपुट: John Doe
}

```

मान लें कि आपकी `config.yaml` फ़ाइल इस प्रकार दिखती है:

```yaml
name: John Doe
age: 30
```

### YAML लिखना

जबकि `yaml` पैकेज पार्सिंग के लिए शानदार है, इसमें YAML लिखने का समर्थन नहीं है। इसके लिए, आपको अपने डेटा को मैनुअली YAML में बदलना पड़ सकता है या यदि उपलब्ध हो, तो किसी अन्य पैकेज का उपयोग करना पड़ सकता है। या, अधिक सरलता से, अपने डेटा परिवर्तनों को प्रबंधित करें और उन्हें YAML सिंटैक्स के अनुरूप स्ट्रिंग के रूप में आउटपुट करें:

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
  print(toYamlString(data)); // आउटपुट: name: Jane Doe
                             //         age: 29
}
```

यह एक प्राथमिक दृष्टिकोण है और जटिल डेटा संरचनाओं या विशेष YAML सुविधाओं के लिए उपयुक्त नहीं हो सकता। जटिल आवश्यकताओं के लिए, आपको एक अधिक व्यापक Dart पैकेज की खोज करनी पड़ सकती है या योगदान देना पड़ सकता है।
