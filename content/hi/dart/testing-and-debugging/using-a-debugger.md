---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:58:27.082554-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: **1. \u092C\u094D\
  \u0930\u0947\u0915\u092A\u0949\u0907\u0902\u091F \u0938\u0947\u091F \u0915\u0930\
  \u0928\u093E:** \u092C\u094D\u0930\u0947\u0915\u092A\u0949\u0907\u0902\u091F \u0938\
  \u0947\u091F \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F, \u092C\u0938\
  \ \u0915\u094B\u0921 \u0932\u093E\u0907\u0928 \u0915\u0947 \u092C\u093E\u0908\u0902\
  \ \u092E\u093E\u0930\u094D\u091C\u093F\u0928 \u092A\u0930 \u0905\u092A\u0928\u0947\
  \ IDE (\u0909\u0926\u093E\u0939\u0930\u0923, \u0935\u093F\u091C\u0941\u0905\u0932\
  \ \u0938\u094D\u091F\u0942\u0921\u093F\u092F\u094B \u0915\u094B\u0921 \u092F\u093E\
  \ \u090F\u0902\u0921\u094D\u0930\u0949\u0907\u0921\u2026"
lastmod: '2024-04-05T22:38:52.714041-06:00'
model: gpt-4-0125-preview
summary: "**1. \u092C\u094D\u0930\u0947\u0915\u092A\u0949\u0907\u0902\u091F \u0938\
  \u0947\u091F \u0915\u0930\u0928\u093E:** \u092C\u094D\u0930\u0947\u0915\u092A\u0949\
  \u0907\u0902\u091F \u0938\u0947\u091F \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\
  \u093F\u090F, \u092C\u0938 \u0915\u094B\u0921 \u0932\u093E\u0907\u0928 \u0915\u0947\
  \ \u092C\u093E\u0908\u0902 \u092E\u093E\u0930\u094D\u091C\u093F\u0928 \u092A\u0930\
  \ \u0905\u092A\u0928\u0947 IDE (\u0909\u0926\u093E\u0939\u0930\u0923, \u0935\u093F\
  \u091C\u0941\u0905\u0932 \u0938\u094D\u091F\u0942\u0921\u093F\u092F\u094B \u0915\
  \u094B\u0921 \u092F\u093E \u090F\u0902\u0921\u094D\u0930\u0949\u0907\u0921 \u0938\
  \u094D\u091F\u0942\u0921\u093F\u092F\u094B) \u092E\u0947\u0902 \u0915\u094D\u0932\
  \u093F\u0915 \u0915\u0930\u0947\u0902 \u091C\u0939\u093E\u0901 \u0906\u092A \u0928\
  \u093F\u0937\u094D\u092A\u093E\u0926\u0928 \u0915\u094B \u0930\u094B\u0915\u0928\
  \u093E \u091A\u093E\u0939\u0924\u0947 \u0939\u0948\u0902\u0964."
title: "\u0921\u0940\u092C\u0917\u0930 \u0915\u093E \u0909\u092A\u092F\u094B\u0917\
  \ \u0915\u0930\u0928\u093E"
weight: 35
---

## कैसे करें:


### मूल डीबगिंग:
**1. ब्रेकपॉइंट सेट करना:** 

ब्रेकपॉइंट सेट करने के लिए, बस कोड लाइन के बाईं मार्जिन पर अपने IDE (उदाहरण, विजुअल स्टूडियो कोड या एंड्रॉइड स्टूडियो) में क्लिक करें जहाँ आप निष्पादन को रोकना चाहते हैं।

```dart
void main() {
  var message = 'Hello, Debugging';
  print(message); // यहां एक ब्रेकपॉइंट सेट करें
}
```

**2. डीबगिंग शुरू करना:**

अपने IDE में, डीबग आइकन पर क्लिक करके या डीबग बटन दबाकर एक डीबगिंग सत्र की शुरुआत करें। ब्रेकपॉइंट्स पर निष्पादन रुक जाएगा।

**3. वेरिएबल्स का निरीक्षण करना:**

एक बार जब निष्पादन रुक जाता है, तो वर्तमान मूल्य देखने के लिए वेरिएबल्स पर होवर करें।

**4. कोड के माध्यम से कदम रखना:**

अपने कोड के माध्यम से एक समय में एक लाइन या फंक्शन के माध्यम से नेविगेट करने के लिए अपने IDE में ओवर कदम, अंदर कदम, और बाहर कदम कमांड्स का उपयोग करें।

### ऑब्ज़र्वेटोरी के साथ उन्नत डीबगिंग:
Dart डार्ट एप्लिकेशन्स को डीबग और प्रोफाइल करने के लिए ऑब्ज़र्वेटोरी नामक एक उपकरण शामिल करता है। यह विशेष रूप से Dart VM पर चलने वाले एप्लिकेशन्स के लिए उपयोगी है।

**ऑब्ज़र्वेटोरी तक पहुँच:**

`--observe` फ्लैग के साथ अपना Dart एप्लिकेशन चलाएँ।

```bash
dart --observe your_program.dart
```

यह कमांड एक URL को कंसोल में प्रिंट करता है, जिसे आप वेब ब्राउज़र में खोलकर ऑब्ज़र्वेटोरी डीबगर तक पहुँच सकते हैं।

### लोकप्रिय तृतीय-पक्ष लाइब्रेरीज़ का उपयोग करना:
फ्लटर एप्लिकेशन्स को डीबग करने के लिए, `flutter_devtools` पैकेज Dart VM और फ्लटर दोनों के साथ एकीकृत प्रदर्शन और डीबगिंग उपकरणों का एक सुइट प्रदान करता है।

**स्थापना:**

सबसे पहले, अपनी `pubspec.yaml` फाइल में `dev_dependencies` के तहत `devtools` जोड़ें:

```yaml
dev_dependencies:
  devtools: any
```

**DevTools लॉन्च करना:**

अपने टर्मिनल में यह कमांड चलाएँ:

```bash
flutter pub global run devtools
```

उसके बाद, डीबग मोड में अपना फ्लटर एप्लिकेशन शुरू करें। DevTools विजेट ट्री विश्लेषण के लिए फ्लटर इंस्पेक्टर, और नेटवर्क गतिविधि की निगरानी के लिए नेटवर्क प्रोफाइलर जैसे फीचर्स प्रदान करता है।

### नमूना आउटपुट:
ब्रेकपॉइंट हिट करने पर, आपका IDE इस प्रकार वेरिएबल मूल्यों और स्टैक ट्रेसेस प्रदर्शित कर सकता है:

```
message: 'Hello, Debugging'
```

Dart में डीबगिंग उपकरणों और तकनीकों को प्रभावी ढंग से लागू करके, डेवलपर्स समस्याओं की पहचान और समाधान अधिक तेज़ी से कर सकते हैं, जिससे विकास प्रक्रिया अधिक सुचारू होती है और अधिक मजबूत एप्लिकेशन बनते हैं।
