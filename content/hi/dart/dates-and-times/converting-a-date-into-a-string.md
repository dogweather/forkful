---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:42.156191-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: Dart \u0924\u093E\
  \u0930\u0940\u0916 \u0914\u0930 \u0938\u092E\u092F \u0938\u0902\u092D\u093E\u0932\
  \u0928\u0947 \u0915\u0947 \u0932\u093F\u090F `DateTime` \u0915\u094D\u0932\u093E\
  \u0938 \u092A\u094D\u0930\u0926\u093E\u0928 \u0915\u0930\u0924\u093E \u0939\u0948\
  , \u0914\u0930 \u0938\u094D\u0935\u0930\u0942\u092A\u0923 \u0915\u0947 \u0932\u093F\
  \u090F `intl` \u092A\u0948\u0915\u0947\u091C \u092A\u094D\u0930\u0926\u093E\u0928\
  \ \u0915\u0930\u0924\u093E \u0939\u0948\u0964 \u0938\u092C\u0938\u0947 \u092A\u0939\
  \u0932\u0947, `intl` \u092A\u0948\u0915\u0947\u091C \u0939\u094B\u0928\u093E\u2026"
lastmod: '2024-03-13T22:44:51.834868-06:00'
model: gpt-4-0125-preview
summary: "Dart \u0924\u093E\u0930\u0940\u0916 \u0914\u0930 \u0938\u092E\u092F \u0938\
  \u0902\u092D\u093E\u0932\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F `DateTime`\
  \ \u0915\u094D\u0932\u093E\u0938 \u092A\u094D\u0930\u0926\u093E\u0928 \u0915\u0930\
  \u0924\u093E \u0939\u0948, \u0914\u0930 \u0938\u094D\u0935\u0930\u0942\u092A\u0923\
  \ \u0915\u0947 \u0932\u093F\u090F `intl` \u092A\u0948\u0915\u0947\u091C \u092A\u094D\
  \u0930\u0926\u093E\u0928 \u0915\u0930\u0924\u093E \u0939\u0948\u0964 \u0938\u092C\
  \u0938\u0947 \u092A\u0939\u0932\u0947, `intl` \u092A\u0948\u0915\u0947\u091C \u0939\
  \u094B\u0928\u093E \u0938\u0941\u0928\u093F\u0936\u094D\u091A\u093F\u0924 \u0915\
  \u0930\u0947\u0902 \u091C\u094B\u0921\u093C\u0915\u0930 `intl."
title: "\u090F\u0915 \u0924\u093E\u0930\u0940\u0916 \u0915\u094B \u0938\u094D\u091F\
  \u094D\u0930\u093F\u0902\u0917 \u092E\u0947\u0902 \u092A\u0930\u093F\u0935\u0930\
  \u094D\u0924\u093F\u0924 \u0915\u0930\u0928\u093E"
weight: 28
---

## कैसे करें:
Dart तारीख और समय संभालने के लिए `DateTime` क्लास प्रदान करता है, और स्वरूपण के लिए `intl` पैकेज प्रदान करता है। सबसे पहले, `intl` पैकेज होना सुनिश्चित करें जोड़कर `intl: ^0.17.0` (या नवीनतम संस्करण) अपनी `pubspec.yaml` फ़ाइल में।

### Dart की कोर लाइब्रेरी का उपयोग करते हुए
```dart
DateTime now = DateTime.now();
String formattedDate = "${now.year}-${now.month}-${now.day}";
print(formattedDate); // आउटपुट: 2023-4-12 (उदहारण के लिए, यह वर्तमान तारीख पर निर्भर करता है)
```

इस उदाहरण में सीधे `DateTime` की गुणों से एक स्ट्रिंग बनाई गई।

### `intl` पैकेज का उपयोग करते हुए
सबसे पहले, पैकेज को आयात करें:

```dart
import 'package:intl/intl.dart';
```

फिर, तारीख को स्वरूपित करें:

```dart
DateTime now = DateTime.now();
String formattedDate = DateFormat('yyyy-MM-dd').format(now);
print(formattedDate); // आउटपुट: 2023-04-12
```

`intl` पैकेज स्थान-विशिष्ट स्वरूपों सहित, आसानी से बहुत अधिक जटिल स्वरूपण की अनुमति देता है:

```dart
String formattedDateLocale = DateFormat.yMMMMd('en_US').format(now);
print(formattedDateLocale); // आउटपुट: April 12, 2023
```

ये उदाहरण दिखाते हैं कि Dart में तारीखों को स्ट्रिंग्स में कैसे बदलना और स्वरूपित करना है, या तो Dart की मूल कार्यक्षमता का उपयोग करके या अधिक उन्नत स्वरूपण विकल्पों के लिए `intl` पैकेज का उपयोग करके।
