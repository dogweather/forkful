---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:38.830137-07:00
description: "\u0915\u0948\u0938\u0947: \u0921\u093E\u0930\u094D\u091F `dart:io` \u0932\
  \u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940 \u0915\u093E \u0909\u092A\u092F\
  \u094B\u0917 \u0915\u0930\u0915\u0947 \u092B\u093C\u093E\u0907\u0932\u094B\u0902\
  \ \u0914\u0930 \u0921\u093E\u092F\u0930\u0947\u0915\u094D\u091F\u0930\u093F\u092F\
  \u094B\u0902 \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0924\
  \u093E \u0939\u0948\u0964 \u092F\u0939 \u090F\u0915 \u0938\u0930\u0932 \u0924\u0930\
  \u0940\u0915\u093E \u0939\u0948 \u092F\u0926\u093F \u090F\u0915 \u0921\u093E\u092F\
  \u0930\u0947\u0915\u094D\u091F\u0930\u0940 \u092E\u094C\u091C\u0942\u0926 \u0939\
  \u0948 \u0924\u094B \u0909\u0938\u0915\u0940 \u091C\u093E\u0901\u091A \u0915\u0930\
  \u0928\u0947\u2026"
lastmod: '2024-03-13T22:44:51.840443-06:00'
model: gpt-4-0125-preview
summary: "\u0921\u093E\u0930\u094D\u091F `dart:io` \u0932\u093E\u0907\u092C\u094D\u0930\
  \u0947\u0930\u0940 \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\
  \u0947 \u092B\u093C\u093E\u0907\u0932\u094B\u0902 \u0914\u0930 \u0921\u093E\u092F\
  \u0930\u0947\u0915\u094D\u091F\u0930\u093F\u092F\u094B\u0902 \u0915\u0947 \u0938\
  \u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0924\u093E \u0939\u0948\u0964 \u092F\
  \u0939 \u090F\u0915 \u0938\u0930\u0932 \u0924\u0930\u0940\u0915\u093E \u0939\u0948\
  \ \u092F\u0926\u093F \u090F\u0915 \u0921\u093E\u092F\u0930\u0947\u0915\u094D\u091F\
  \u0930\u0940 \u092E\u094C\u091C\u0942\u0926 \u0939\u0948 \u0924\u094B \u0909\u0938\
  \u0915\u0940 \u091C\u093E\u0901\u091A \u0915\u0930\u0928\u0947 \u0915\u093E."
title: "\u0921\u093E\u092F\u0930\u0947\u0915\u094D\u091F\u0930\u0940 \u092E\u094C\u091C\
  \u0942\u0926 \u0939\u0948 \u092F\u093E \u0928\u0939\u0940\u0902, \u091C\u093E\u0902\
  \u091A\u0928\u093E"
weight: 20
---

## कैसे:
डार्ट `dart:io` लाइब्रेरी का उपयोग करके फ़ाइलों और डायरेक्टरियों के साथ काम करता है। यह एक सरल तरीका है यदि एक डायरेक्टरी मौजूद है तो उसकी जाँच करने का:

```dart
import 'dart:io';

void main() {
  var directory = Directory('path/to/your/directory');

  if (directory.existsSync()) {
    print('Directory exists');
  } else {
    print('Directory does not exist');
  }
}
```
यदि डायरेक्टरी मौजूद हो तो नमूना उत्पादन:
```
Directory exists
```

या, यदि वह मौजूद नहीं है:
```
Directory does not exist
```

अधिक जटिल परिदृश्यों को संभालने के लिए, जैसे कि असिंक्रोनस जाँच करना या यदि डायरेक्टरी मौजूद नहीं है तो उसे बनाना, आप निम्नलिखित दृष्टिकोण का उपयोग कर सकते हैं:

```dart
import 'dart:io';

void main() async {
  var directory = Directory('path/to/your/directory');

  // असिंक्रोनस रूप से जाँचें कि डायरेक्टरी मौजूद है या नहीं
  var exists = await directory.exists();
  if (exists) {
    print('Directory exists');
  } else {
    print('Directory does not exist, creating...');
    await directory.create(); // यह डायरेक्टरी को बनाता है
    print('Directory created');
  }
}
```

नमूना उत्पादन यदि डायरेक्टरी मौजूद नहीं थी और बनाई गई थी:
```
Directory does not exist, creating...
Directory created
```

डार्ट की बिल्ट-इन क्षमताएं आमतौर पर फ़ाइलों और डायरेक्टरियों को संभालने के लिए पर्याप्त होती हैं, इसलिए इस कार्य के लिए तृतीय-पक्ष लाइब्रेरियों की आमतौर पर आवश्यकता नहीं होती है। हालाँकि, अधिक जटिल फाइल सिस्टम ऑपरेशनों के लिए, `path` जैसे पैकेज (प्लेटफार्म-अज्ञेयवादी तरीके से पथों को संभालने के लिए) `dart:io` लाइब्रेरी को पूरक कर सकते हैं लेकिन सीधे तौर पर वह अधिक उन्नत डायरेक्टरी अस्तित्व जाँच की पेशकश नहीं करते हैं जो दिखाया गया है।
