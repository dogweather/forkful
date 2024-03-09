---
title:                "यादृच्छिक संख्याओं का उत्पादन"
date:                  2024-03-08T21:54:55.241707-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
Dart में यादृच्छिक संख्याएँ उत्पन्न करने का अर्थ है, ऐसे अंकों की रचना करना जो अप्रत्याशित हों और प्रत्येक निष्पादन पर भिन्न हों। प्रोग्रामर्स इस कार्यक्षमता का उपयोग विविध कारणों से करते हैं, परीक्षण वातावरण में वास्तविक दुनिया की परिस्थितियों का अनुकरण करने से लेकर गेम मैकेनिक्स को सक्षम करने और क्रिप्टोग्राफिक ऑपरेशनों में यादृच्छिकता के माध्यम से सुरक्षा सुनिश्चित करने तक।

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