---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:16.386443-07:00
description: "\u0921\u093E\u0930\u094D\u091F \u092E\u0947\u0902 \u090F\u0915 \u0938\
  \u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0938\u0947 \u0909\u0926\u094D\u0927\
  \u0930\u0923 \u091A\u093F\u0939\u094D\u0928 \u0939\u091F\u093E\u0928\u0947 \u0915\
  \u093E \u092E\u0924\u0932\u092C \u0939\u0948 \u0921\u092C\u0932 (\") \u092F\u093E\
  \ \u0938\u093F\u0902\u0917\u0932 (') \u0909\u0926\u094D\u0927\u0930\u0923 \u091A\
  \u093F\u0939\u094D\u0928\u094B\u0902 \u0915\u094B \u0938\u094D\u091F\u094D\u0930\
  \u093F\u0902\u0917 \u0915\u0947 \u0936\u0941\u0930\u0941\u0906\u0924 \u0914\u0930\
  \ \u0905\u0902\u0924 \u0938\u0947 \u0928\u093F\u0915\u093E\u0932\u0928\u093E, \u091C\
  \u094B \u0921\u0947\u091F\u093E \u0938\u092B\u093E\u0908 \u092F\u093E\u2026"
lastmod: '2024-03-11T00:14:25.665589-06:00'
model: gpt-4-0125-preview
summary: "\u0921\u093E\u0930\u094D\u091F \u092E\u0947\u0902 \u090F\u0915 \u0938\u094D\
  \u091F\u094D\u0930\u093F\u0902\u0917 \u0938\u0947 \u0909\u0926\u094D\u0927\u0930\
  \u0923 \u091A\u093F\u0939\u094D\u0928 \u0939\u091F\u093E\u0928\u0947 \u0915\u093E\
  \ \u092E\u0924\u0932\u092C \u0939\u0948 \u0921\u092C\u0932 (\") \u092F\u093E \u0938\
  \u093F\u0902\u0917\u0932 (') \u0909\u0926\u094D\u0927\u0930\u0923 \u091A\u093F\u0939\
  \u094D\u0928\u094B\u0902 \u0915\u094B \u0938\u094D\u091F\u094D\u0930\u093F\u0902\
  \u0917 \u0915\u0947 \u0936\u0941\u0930\u0941\u0906\u0924 \u0914\u0930 \u0905\u0902\
  \u0924 \u0938\u0947 \u0928\u093F\u0915\u093E\u0932\u0928\u093E, \u091C\u094B \u0921\
  \u0947\u091F\u093E \u0938\u092B\u093E\u0908 \u092F\u093E\u2026"
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0938\u0947 \u0915\u094B\
  \u091F\u094D\u0938 \u0939\u091F\u093E\u0928\u093E"
---

{{< edit_this_page >}}

## क्या और क्यों?
डार्ट में एक स्ट्रिंग से उद्धरण चिह्न हटाने का मतलब है डबल (") या सिंगल (') उद्धरण चिह्नों को स्ट्रिंग के शुरुआत और अंत से निकालना, जो डेटा सफाई या स्ट्रिंग्स को आगे की प्रोसेसिंग के लिए तैयार करने के लिए उपयोगी होता है। प्रोग्रामर इसे डेटा इनपुट्स को सामान्यीकृत करने, डेटा स्टोरेज में एकरूपता सुनिश्चित करने, या ऐसे APIs के साथ इंटरफेसिंग करते समय करते हैं जो उद्धृत प्रारूपों में डेटा वापस कर सकते हैं।

## कैसे:
डार्ट बिना तीसरे पक्ष की लाइब्रेरियों की आवश्यकता के, स्ट्रिंग से उद्धरण चिह्न हटाने के लिए बिल्ट-इन स्ट्रिंग मेथड्स का उपयोग करने के सीधे तरीके प्रदान करता है।

### उदाहरण 1: `replaceFirst` और `replaceAll` का उपयोग करके
अगर आप ऐसी स्ट्रिंग्स से निपट रहे हैं जो उद्धरण चिह्न से शुरू और समाप्त होती हैं, तो आप उन्हें हटाने के लिए `replaceFirst` और `replaceAll` मेथड्स का उपयोग कर सकते हैं।

```dart
String quotedString = '"Hello, World!"';
String singleQuotedString = '\'Dart Programming\'';

// डबल उद्धरण चिह्न हटाना
String noDoubleQuotes = quotedString.replaceFirst('"', '').replaceAll('"', '');
print(noDoubleQuotes); // आउटपुट: Hello, World!

// सिंगल उद्धरण चिह्न हटाना
String noSingleQuotes = singleQuotedString.replaceFirst('\'', '').replaceAll('\'', '');
print(noSingleQuotes); // आउटपुट: Dart Programming
```

### उदाहरण 2: `substring` का उपयोग करके
यह तरीका उस समय उपयोगी होता है जब आपको पक्का पता हो कि उद्धरण चिह्न स्ट्रिंग की बिल्कुल शुरुआत और अंत में हैं।

```dart
String quotedString = '"Flutter Development"';
// त्रुटियों से बचने के लिए उद्धरण चिह्न हटाने से पहले जांच करें
if (quotedString.startsWith('"') && quotedString.endsWith('"')) {
  quotedString = quotedString.substring(1, quotedString.length - 1);
}
print(quotedString); // आउटपुट: Flutter Development
```

### उदाहरण 3: कस्टम एक्सटेंशन मेथड
यदि आपकी परियोजना में बार-बार उद्धरण चिह्न हटाने की आवश्यकता होती है, तो अधिक पुन: उपयोगिता के लिए `String` पर एक कस्टम एक्सटेंशन बनाने पर विचार करें।

```dart
extension UnquoteString on String {
  String unquote() {
    var str = this;
    if (str.startsWith('"') && str.endsWith('"') || str.startsWith('\'') && str.endsWith('\'')) {
      str = str.substring(1, str.length - 1);
    }
    return str;
  }
}

void main() {
  String doubleQuoted = '"This is Dart"';
  String singleQuoted = '\'This is awesome\'';
  print(doubleQuoted.unquote()); // आउटपुट: This is Dart
  print(singleQuoted.unquote()); // आउटपुट: This is awesome
}
```

ये दृष्टिकोण आपको डार्ट में स्ट्रिंग्स से उद्धरण चिह्नों को प्रभावी ढंग से हटाने में मदद करना चाहिए, आपकी डेटा प्रोसेसिंग और तैयारी के कार्यप्रवाहों को बेहतर बनाते हुए।
