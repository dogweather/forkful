---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:55.241707-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: Dart \u0915\u0940\
  \ \u092E\u0942\u0932 \u0932\u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940 \u092E\
  \u0947\u0902 `dart:math` \u092E\u0947\u0902 \u092A\u093E\u0908 \u091C\u093E\u0928\
  \u0947 \u0935\u093E\u0932\u0940 `Random` \u0915\u094D\u0932\u093E\u0938 \u0915\u093E\
  \ \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\u0947 \u092F\u093E\u0926\u0943\
  \u091A\u094D\u091B\u093F\u0915 \u0938\u0902\u0916\u094D\u092F\u093E\u0913\u0902\
  \ \u0915\u094B \u0909\u0924\u094D\u092A\u0928\u094D\u0928 \u0915\u0930\u0928\u0947\
  \ \u0915\u0947 \u0932\u093F\u090F \u0938\u092E\u0930\u094D\u0925\u0928 \u0936\u093E\
  \u092E\u093F\u0932 \u0939\u0948\u0964\u2026"
lastmod: '2024-03-13T22:44:51.805774-06:00'
model: gpt-4-0125-preview
summary: "Dart \u0915\u0940 \u092E\u0942\u0932 \u0932\u093E\u0907\u092C\u094D\u0930\
  \u0947\u0930\u0940 \u092E\u0947\u0902 `dart:math` \u092E\u0947\u0902 \u092A\u093E\
  \u0908 \u091C\u093E\u0928\u0947 \u0935\u093E\u0932\u0940 `Random` \u0915\u094D\u0932\
  \u093E\u0938 \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\u0947\
  \ \u092F\u093E\u0926\u0943\u091A\u094D\u091B\u093F\u0915 \u0938\u0902\u0916\u094D\
  \u092F\u093E\u0913\u0902 \u0915\u094B \u0909\u0924\u094D\u092A\u0928\u094D\u0928\
  \ \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u0938\u092E\u0930\u094D\
  \u0925\u0928 \u0936\u093E\u092E\u093F\u0932 \u0939\u0948\u0964 \u092F\u0939\u093E\
  \u0901 \u090F\u0915 \u092E\u0942\u0932 \u0909\u0926\u093E\u0939\u0930\u0923 \u0939\
  \u0948."
title: "\u092F\u093E\u0926\u0943\u091A\u094D\u091B\u093F\u0915 \u0938\u0902\u0916\u094D\
  \u092F\u093E\u0913\u0902 \u0915\u093E \u0909\u0924\u094D\u092A\u093E\u0926\u0928"
weight: 12
---

## कैसे करें:
Dart की मूल लाइब्रेरी में `dart:math` में पाई जाने वाली `Random` क्लास का उपयोग करके यादृच्छिक संख्याओं को उत्पन्न करने के लिए समर्थन शामिल है। यहाँ एक मूल उदाहरण है:

```dart
import 'dart:math';

void main() {
  var rand = Random();
  int randomNumber = rand.nextInt(100); // 0 और 99 के बीच एक यादृच्छिक पूर्णांक उत्पन्न करता है
  double randomDouble = rand.nextDouble(); // 0.0 और 1.0 के बीच एक यादृच्छिक दोहरी संख्या उत्पन्न करता है
  print(randomNumber);
  print(randomDouble);
}
```

*नमूना आउटपुट: (यह प्रत्येक समय चलाने पर भिन्न होगा)*

```
23
0.6722390975465775
```

क्रिप्टोग्राफिक यादृच्छिकता की आवश्यकता वाले उपयोग के मामलों के लिए, Dart `Random.secure` निर्माता प्रदान करता है:

```dart
import 'dart:math';

void main() {
  var secureRand = Random.secure();
  int secureRandomNumber = secureRand.nextInt(100);
  print(secureRandomNumber);
}
```

*नमूना आउटपुट: (यह प्रत्येक समय चलाने पर भिन्न होगा)*

```
45
```

यदि आप Flutter प्रोजेक्ट्स पर काम कर रहे हैं या अधिक जटिल यादृच्छिकता की आवश्यकता है, तो आपको नाम, पते और तारीखों जैसे विविध प्रकार के यादृच्छिक डेटा उत्पन्न करने के लिए `faker` पैकेज उपयोगी लग सकता है।

`faker` का उपयोग करने के लिए, सबसे पहले, इसे अपनी `pubspec.yaml` फाइल में जोड़ें:

```yaml
dependencies:
  faker: ^2.0.0
```

फिर, इसे निम्नलिखित दिखाए गए अनुसार आयात करें और उपयोग करें:

```dart
import 'package:faker/faker.dart';

void main() {
  final faker = Faker();
  print(faker.person.name()); // एक यादृच्छिक नाम उत्पन्न करता है
  print(faker.address.city()); // एक यादृच्छिक शहर का नाम उत्पन्न करता है
}
```

*नमूना आउटपुट:*

```
Josie Runolfsdottir
East Lysanne
```
