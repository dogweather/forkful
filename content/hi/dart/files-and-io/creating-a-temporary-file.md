---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:58.014752-07:00
description: "\u0915\u0948\u0938\u0947: \u0921\u093E\u0930\u094D\u091F \u0915\u0940\
  \ `dart:io` \u0932\u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940 \u0905\u0938\u094D\
  \u0925\u093E\u092F\u0940 \u092B\u093C\u093E\u0907\u0932\u094B\u0902 \u0915\u094B\
  \ \u092C\u0928\u093E\u0928\u0947 \u092E\u0947\u0902 `Directory` \u0915\u094D\u0932\
  \u093E\u0938 \u0915\u0947 \u092E\u093E\u0927\u094D\u092F\u092E \u0938\u0947 \u0938\
  \u0941\u0935\u093F\u0927\u093E \u092A\u094D\u0930\u0926\u093E\u0928 \u0915\u0930\
  \u0924\u0940 \u0939\u0948\u0964 \u092F\u0939\u093E\u0901 \u090F\u0915 \u0905\u0938\
  \u094D\u0925\u093E\u092F\u0940 \u092B\u093C\u093E\u0907\u0932 \u092C\u0928\u093E\
  \u0928\u0947 \u0914\u0930 \u0909\u0938\u092E\u0947\u0902\u2026"
lastmod: '2024-03-13T22:44:51.849265-06:00'
model: gpt-4-0125-preview
summary: "\u0921\u093E\u0930\u094D\u091F \u0915\u0940 `dart:io` \u0932\u093E\u0907\
  \u092C\u094D\u0930\u0947\u0930\u0940 \u0905\u0938\u094D\u0925\u093E\u092F\u0940\
  \ \u092B\u093C\u093E\u0907\u0932\u094B\u0902 \u0915\u094B \u092C\u0928\u093E\u0928\
  \u0947 \u092E\u0947\u0902 `Directory` \u0915\u094D\u0932\u093E\u0938 \u0915\u0947\
  \ \u092E\u093E\u0927\u094D\u092F\u092E \u0938\u0947 \u0938\u0941\u0935\u093F\u0927\
  \u093E \u092A\u094D\u0930\u0926\u093E\u0928 \u0915\u0930\u0924\u0940 \u0939\u0948\
  \u0964 \u092F\u0939\u093E\u0901 \u090F\u0915 \u0905\u0938\u094D\u0925\u093E\u092F\
  \u0940 \u092B\u093C\u093E\u0907\u0932 \u092C\u0928\u093E\u0928\u0947 \u0914\u0930\
  \ \u0909\u0938\u092E\u0947\u0902 \u0915\u0941\u091B \u0938\u093E\u092E\u0917\u094D\
  \u0930\u0940 \u0932\u093F\u0916\u0928\u0947 \u0915\u093E \u090F\u0915 \u0938\u0930\
  \u0932 \u0924\u0930\u0940\u0915\u093E \u0939\u0948."
title: "\u0905\u0938\u094D\u0925\u093E\u092F\u0940 \u092B\u093C\u093E\u0907\u0932\
  \ \u092C\u0928\u093E\u0928\u093E"
weight: 21
---

## कैसे:
डार्ट की `dart:io` लाइब्रेरी अस्थायी फ़ाइलों को बनाने में `Directory` क्लास के माध्यम से सुविधा प्रदान करती है। यहाँ एक अस्थायी फ़ाइल बनाने और उसमें कुछ सामग्री लिखने का एक सरल तरीका है:

```dart
import 'dart:io';

Future<void> main() async {
  // एक अस्थायी डायरेक्टरी बनाएं (सिस्टम-विशिष्ट स्थान)
  Directory tempDir = await Directory.systemTemp.createTemp('my_temp_dir_');

  // उस डायरेक्टरी के भीतर एक अस्थायी फ़ाइल बनाएं
  File tempFile = File('${tempDir.path}/my_temp_file.txt');

  // अस्थायी फ़ाइल में कुछ सामग्री लिखें
  await tempFile.writeAsString('This is some temporary content');

  print('अस्थायी फ़ाइल बनाई गई: ${tempFile.path}');

  // नमूना आउटपुट: अस्थायी फ़ाइल बनाई गई: /tmp/my_temp_dir_A1B2C3/my_temp_file.txt
}
```

### तृतीय-पक्ष लाइब्रेरी का उपयोग करना: `path_provider`
विशेष रूप से ऐप्लिकेशन (खासकर Flutter के साथ मोबाइल ऐप्स) के लिए, आप एक अधिक एकीकृत और प्रबंधनीय तरीके से अस्थायी फ़ाइलें बनाना चाह सकते हैं। `path_provider` पैकेज अलग-अलग प्लेटफॉर्मों (iOS, Android, आदि) पर सही अस्थायी डायरेक्टरी खोजने में आपकी मदद कर सकता है।

पहले, अपने `pubspec.yaml` में निर्भरताओं के अंतर्गत `path_provider` जोड़ें:

```yaml
dependencies:
  path_provider: ^2.0.9
```

और यहाँ है कि कैसे आप इसका उपयोग कर एक अस्थायी फ़ाइल बना सकते हैं:

```dart
import 'dart:io';
import 'package:path_provider/path_provider.dart';

Future<void> main() async {
  // अस्थायी डायरेक्टरी प्राप्त करें
  final Directory tempDir = await getTemporaryDirectory();

  // उस डायरेक्टरी के भीतर एक अस्थायी फ़ाइल बनाएं
  final File tempFile = File('${tempDir.path}/my_temp_file.txt');

  // अस्थायी फ़ाइल में कुछ सामग्री लिखें
  await tempFile.writeAsString('This is some temporary content with path_provider');

  print('path_provider के साथ अस्थायी फ़ाइल बनाई गई: ${tempFile.path}');

  // नमूना आउटपुट: path_provider के साथ अस्थायी फ़�इल बनाई गई: /tmp/my_temp_file.txt (पथ प्लेटफ़ॉर्म द्वारा भिन्न हो सकता है)
}
```

ये स्निपेट्स Dart में अस्थायी फ़ाइलों को बनाने और उनके साथ बातचीत करने का वर्णन करते हैं, जो अल्पकालिक उद्देश्यों के लिए डाटा प्रबंधन के लिए एक सीधा और व्यावहारिक दृष्टिकोण प्रदान करता है।
