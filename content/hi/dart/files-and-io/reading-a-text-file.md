---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:55:46.999540-07:00
description: "Dart \u092E\u0947\u0902 \u091F\u0947\u0915\u094D\u0938\u094D\u091F \u092B\
  \u093E\u0907\u0932 \u0915\u094B \u092A\u0922\u093C\u0928\u093E \u092B\u093E\u0907\
  \u0932 \u0938\u093F\u0938\u094D\u091F\u092E \u092A\u0930 \u0938\u0902\u0917\u094D\
  \u0930\u0939\u093F\u0924 \u092B\u093E\u0907\u0932\u094B\u0902 \u0938\u0947 \u0921\
  \u0947\u091F\u093E \u092A\u094D\u0930\u093E\u092A\u094D\u0924 \u0915\u0930\u0928\
  \u0947 \u0914\u0930 \u0909\u0938\u0947 \u090F\u0915\u094D\u0938\u0947\u0938 \u0915\
  \u0930\u0928\u0947 \u0915\u0940 \u092A\u094D\u0930\u0915\u094D\u0930\u093F\u092F\
  \u093E \u0939\u094B\u0924\u0940 \u0939\u0948\u0964 \u092A\u094D\u0930\u094B\u0917\
  \u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0907\u0938 \u0915\u094D\u0930\u093F\
  \u092F\u093E \u0915\u094B\u2026"
lastmod: '2024-03-13T22:44:51.845711-06:00'
model: gpt-4-0125-preview
summary: "Dart \u092E\u0947\u0902 \u091F\u0947\u0915\u094D\u0938\u094D\u091F \u092B\
  \u093E\u0907\u0932 \u0915\u094B \u092A\u0922\u093C\u0928\u093E \u092B\u093E\u0907\
  \u0932 \u0938\u093F\u0938\u094D\u091F\u092E \u092A\u0930 \u0938\u0902\u0917\u094D\
  \u0930\u0939\u093F\u0924 \u092B\u093E\u0907\u0932\u094B\u0902 \u0938\u0947 \u0921\
  \u0947\u091F\u093E \u092A\u094D\u0930\u093E\u092A\u094D\u0924 \u0915\u0930\u0928\
  \u0947 \u0914\u0930 \u0909\u0938\u0947 \u090F\u0915\u094D\u0938\u0947\u0938 \u0915\
  \u0930\u0928\u0947 \u0915\u0940 \u092A\u094D\u0930\u0915\u094D\u0930\u093F\u092F\
  \u093E \u0939\u094B\u0924\u0940 \u0939\u0948\u0964 \u092A\u094D\u0930\u094B\u0917\
  \u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0907\u0938 \u0915\u094D\u0930\u093F\
  \u092F\u093E \u0915\u094B\u2026"
title: "\u091F\u0947\u0915\u094D\u0938\u094D\u091F \u092B\u093C\u093E\u0907\u0932\
  \ \u092A\u0922\u093C\u0928\u093E"
---

{{< edit_this_page >}}

## क्या और क्यों?

Dart में टेक्स्ट फाइल को पढ़ना फाइल सिस्टम पर संग्रहित फाइलों से डेटा प्राप्त करने और उसे एक्सेस करने की प्रक्रिया होती है। प्रोग्रामर्स इस क्रिया को इनपुट डेटा, कॉन्फ़िगुरेशन सेटिंग्स, या डेटासेट्स को पढ़ने के लिए करते हैं, जो कि सिम्पल स्क्रिप्ट्स से लेकर जटिल ऐप्स तक कई अनुप्रयोगों के लिए एक मौलिक ऑपरेशन होता है।

## कैसे करें:

Dart की कोर लाइब्रेरी, `dart:io`, टेक्स्ट फाइलों को सिंक्रोनसली या असिंक्रोनसली पढ़ने की जरूरी कार्यक्षमताएं प्रदान करती है। आइए देखें कैसे।

**सिंक्रोनसली:**

```dart
import 'dart:io';

void main() {
  var fileName = "path/to/your/textfile.txt";
  var file = File(fileName);

  // फाइल को सिंक्रोनसली पढ़ना
  var contents;
  try {
    contents = file.readAsStringSync();
    print(contents);
  } catch (e) {
    print('Error reading file: $e');
  }
}
```

**असिंक्रोनसली:**

खासकर बड़ी फाइलों या उत्तरदायी ऐप्लिकेशंस के लिए, प्रोग्राम को फाइल पढ़ते समय ब्लॉक करने से बचने के लिए:

```dart
import 'dart:io';

void main() async {
  var fileName = "path/to/your/textfile.txt";
  var file = File(fileName);

  try {
    String contents = await file.readAsString();
    print(contents);
  } catch (e) {
    print('Error reading file: $e');
  }
}
```

**नमूना आउटपुट:**

यदि आपकी टेक्स्ट फाइल में शामिल है:

```
Hello, Dart!
```

ऊपर दिये गये दोनों तरीकों से आउटपुट होगा:

```
Hello, Dart!
```

**तृतीय-पक्ष पुस्तकालय का उपयोग करना:**

सरलीकृत फाइल ऑपरेशंस या बढ़ाए हुए त्रुटि संभाल सुविधाओं जैसी अतिरिक्त विशेषताओं के लिए, आप `पैकेज:फाइल` जैसे तृतीय-पक्ष पुस्तकालयों पर विचार कर सकते हैं। हालांकि, मेरे आखिरी अपडेट के रूप में, ऊपर दिखाए गए तरीके के रूप में, मुख्य `dart:io` पैकेज का सीधे तौर पर उपयोग करना, Dart में टेक्स्ट फाइलों को पढ़ने के लिए सबसे आम और सरल तरीका है।
