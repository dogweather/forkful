---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:40.486099-07:00
description: "\u0921\u093E\u0930\u094D\u091F(Dart) \u092E\u0947\u0902 \u0928\u093F\
  \u092F\u092E\u093F\u0924 \u0905\u092D\u093F\u0935\u094D\u092F\u0915\u094D\u0924\u093F\
  \u092F\u094B\u0902 (regex) \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u090F\u0915\
  \ \u0936\u0915\u094D\u0924\u093F\u0936\u093E\u0932\u0940 \u0924\u0930\u0940\u0915\
  \u093E \u092A\u094D\u0930\u0926\u093E\u0928 \u0915\u0930\u0924\u093E \u0939\u0948\
  \ \u091C\u094B \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u0916\
  \u094B\u091C\u0928\u0947 \u0914\u0930 \u092E\u0948\u0928\u093F\u092A\u0941\u0932\
  \u0947\u091F \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F, \u092A\u094D\
  \u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0915\u094B\u2026"
lastmod: '2024-03-09T21:06:15.908060-07:00'
model: gpt-4-0125-preview
summary: "\u0921\u093E\u0930\u094D\u091F(Dart) \u092E\u0947\u0902 \u0928\u093F\u092F\
  \u092E\u093F\u0924 \u0905\u092D\u093F\u0935\u094D\u092F\u0915\u094D\u0924\u093F\u092F\
  \u094B\u0902 (regex) \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u090F\u0915 \u0936\
  \u0915\u094D\u0924\u093F\u0936\u093E\u0932\u0940 \u0924\u0930\u0940\u0915\u093E\
  \ \u092A\u094D\u0930\u0926\u093E\u0928 \u0915\u0930\u0924\u093E \u0939\u0948 \u091C\
  \u094B \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u0916\u094B\
  \u091C\u0928\u0947 \u0914\u0930 \u092E\u0948\u0928\u093F\u092A\u0941\u0932\u0947\
  \u091F \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F, \u092A\u094D\u0930\
  \u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0915\u094B\u2026"
title: "\u0930\u0947\u0917\u0941\u0932\u0930 \u090F\u0915\u094D\u0938\u092A\u094D\u0930\
  \u0947\u0936\u0928 \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0928\
  \u093E"
---

{{< edit_this_page >}}

## क्या और क्यों?
डार्ट(Dart) में नियमित अभिव्यक्तियों (regex) का उपयोग एक शक्तिशाली तरीका प्रदान करता है जो स्ट्रिंग को खोजने और मैनिपुलेट करने के लिए, प्रोग्रामर्स को जटिल पाठ संसाधन कार्यों को कुशलता से करने में सक्षम बनाता है। Regex को समझने से, डेवलपर्स पाठ मान्यताओं, खोज पैटर्न, और पाठ परिवर्तनों को जल्दी से कर सकते हैं, जो फॉर्म्स की प्रोसेसिंग, डेटा पार्सिंग, और आधुनिक एप्लीकेशनों में सामान्य स्ट्रिंग मैनिपुलेशन्स के लिए आवश्यक होती है।

## कैसे:
डार्ट नियमित अभिव्यक्तियों के लिए `RegExp` क्लास का उपयोग करता है। यहाँ एक साधारण पैटर्न को स्ट्रिंग के भीतर मैच करने का एक मूल उदाहरण है:

```dart
void main() {
  var pattern = RegExp(r'\bDart\b');
  var text = 'Learning Dart programming is exciting.';

  if (pattern.hasMatch(text)) {
    print('मैच मिला!');
  } else {
    print('कोई मैच नहीं मिला।');
  }
  // आउटपुट: मैच मिला!
}
```

एक स्ट्रिंग से मैचेस को निकालने के लिए, आप `allMatches` मेथड का उपयोग कर सकते हैं। यह मेथड मैचों का एक इटरेबल लौटाती है:

```dart
void main() {
  var pattern = RegExp(r'\b\w+\b');
  var text = 'Dart is awesome!';

  var matches = pattern.allMatches(text);
  for (final match in matches) {
    print(match.group(0)); // यह मैच किए गए सबस्ट्रिंग्स को प्रिंट करता है।
  }
  // आउटपुट:
  // Dart
  // is
  // awesome
}
```

पाठ को बदलना `replaceFirst` या `replaceAll` मेथडों का उपयोग करके प्राप्त किया जा सकता है:

```dart
void main() {
  var pattern = RegExp(r'\bDart\b');
  var text = 'Dart is not just a dart.';
  
  // पहली घटना को बदलें
  var modifiedText = text.replaceFirst(pattern, 'Flutter');
  print(modifiedText); 
  // आउटपुट: Flutter is not just a dart.

  // सभी घटनाओं को बदलें
  modifiedText = text.replaceAll(pattern, 'Flutter');
  print(modifiedText);
  // आउटपुट: Flutter is not just a flutter.
}
```

एक स्ट्रिंग को एक regex पैटर्न द्वारा विभाजित करना `split` मेथड का उपयोग करके सीधा है:

```dart
void main() {
  var pattern = RegExp(r'\s+'); // किसी भी स्पेस वाले चरित्र को मैच करता है
  var text = 'Dart is fun';

  var parts = text.split(pattern);
  print(parts); 
  // आउटपुट: [Dart, is, fun]
}
```

डार्ट की `RegExp` द्वारा सीधे समर्थित नहीं की गई जटिल पार्सिंग या मान्यकरण के लिए, आपको तीसरे पक्ष की लाइब्रेरियों पर विचार करना पड़ सकता है, लेकिन डार्ट की स्टैंडर्ड लाइब्रेरी अक्सर सामान्य regex कार्यों के लिए पर्याप्त होती है, जो नियमित अभिव्यक्तियों को संभालने में इसकी उपयोगिता और बहुमुखी प्रतिभा पर जोर देती है।
