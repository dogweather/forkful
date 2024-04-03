---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:58:25.072560-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: \u0921\u093E\u0930\
  \u094D\u091F \u092E\u0947\u0902, `dart:io` \u092E\u0947\u0902 \u0909\u092A\u0932\
  \u092C\u094D\u0927 `stderr` \u0911\u092C\u094D\u091C\u0947\u0915\u094D\u091F \u0915\
  \u093E \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0915\u0930\u0915\u0947\
  \ stderr \u092E\u0947\u0902 \u0932\u093F\u0916\u0928\u093E \u0938\u0940\u0927\u093E\
  \ \u0939\u0948\u0964 \u092F\u0939\u093E\u0901 \u090F\u0915 \u092E\u0942\u0932 \u0909\
  \u0926\u093E\u0939\u0930\u0923 \u0939\u0948."
lastmod: '2024-03-13T22:44:51.843950-06:00'
model: gpt-4-0125-preview
summary: "\u0921\u093E\u0930\u094D\u091F \u092E\u0947\u0902, `dart:io` \u092E\u0947\
  \u0902 \u0909\u092A\u0932\u092C\u094D\u0927 `stderr` \u0911\u092C\u094D\u091C\u0947\
  \u0915\u094D\u091F \u0915\u093E \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932\
  \ \u0915\u0930\u0915\u0947 stderr \u092E\u0947\u0902 \u0932\u093F\u0916\u0928\u093E\
  \ \u0938\u0940\u0927\u093E \u0939\u0948\u0964 \u092F\u0939\u093E\u0901 \u090F\u0915\
  \ \u092E\u0942\u0932 \u0909\u0926\u093E\u0939\u0930\u0923 \u0939\u0948."
title: "\u092E\u093E\u0928\u0915 \u0924\u094D\u0930\u0941\u091F\u093F \u0915\u0947\
  \ \u0932\u093F\u090F \u0932\u093F\u0916\u0928\u093E"
weight: 25
---

## कैसे करें:
डार्ट में, `dart:io` में उपलब्ध `stderr` ऑब्जेक्ट का इस्तेमाल करके stderr में लिखना सीधा है। यहाँ एक मूल उदाहरण है:
```dart
import 'dart:io';

void main() {
  stderr.writeln('This is an error message.');
}
```

जब चलाया जाता है तो आउटपुट:
```
This is an error message.
```
यह संदेश stderr स्ट्रीम में भेजा जाता है, जिसे आमतौर पर कंसोल या टर्मिनल में प्रदर्शित किया जाता है।

जैसे कि एक अपवाद को लॉगिंग करना, जटिलता का प्रदर्शन करने के लिए, डार्ट के अमीर फीचर सेट त्रुटियों के संचालन को संक्षिप्त और प्रभावी बनाने की अनुमति देते हैं:
```dart
import 'dart:io';

void riskyOperation() {
  try {
    // एक ऐसे ऑपरेशन का अनुकरण करें जो फेंक सकता है
    throw Exception('Something went wrong!');
  } catch (e) {
    stderr.writeln('Error: $e');
  }
}

void main() {
  riskyOperation();
}
```

जब चलाया जाता है तो आउटपुट:
```
Error: Exception: Something went wrong!
```

यह पैटर्न विशेष रूप से उन एप्लिकेशनों के लिए उपयोगी है जिन्हें सामान्य लॉग्स को त्रुटि लॉग्स से अलग करने की आवश्यकता होती है, जिससे एप्लिकेशनों की निगरानी और डिबगिंग आसान हो जाती है।

हालांकि डार्ट की स्टैंडर्ड लाइब्रेरी काफी व्यापक है, बहुत सारे प्रोग्रामों को stderr में लिखने के लिए तृतीय-पक्ष लाइब्रेरियों की आवश्यकता नहीं होती है। हालाँकि, यदि आपके एप्लिकेशन को अधिक सोफ़िस्टिकेटेड लॉगिंग क्षमताओं (उदाहरण के लिए, फाइलों में, नेटवर्क पर, फॉर्मेटिंग) की आवश्यकता हो, तो `logging` पैकेज एक लोकप्रिय विकल्प है। यहाँ त्रुटियों के लिए `logging` का उपयोग करने की एक त्वरित झलक है:
```dart
import 'dart:io';
import 'package:logging/logging.dart';

final logger = Logger('MyAppLogger');

void setupLogging() {
  logger.onRecord.listen((record) {
    if (record.level >= Level.SEVERE) {
      stderr.writeln('${record.level.name}: ${record.time}: ${record.message}');
    }
  });
}

void main() {
  setupLogging();
  logger.severe('Severe Error: Something significantly bad happened.');
}
```

जब चलाया जाता है तो आउटपुट:
```
SEVERE: 2023-04-01 00:00:00.000: Severe Error: Something significantly bad happened.
```

यह विधि त्रुटियों के रूप में क्या लॉग किया जाता है और उन्हें कैसे फॉरमेट किया जाता है को नियंत्रित करने की एक उच्च स्तर की अनुकूलन और नियंत्रण प्रदान करता है, जो बड़े, अधिक जटिल एप्लिकेशनों में बहुत मददगार हो सकता है।
