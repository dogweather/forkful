---
title:                "रेगुलर एक्सप्रेशन का उपयोग करना"
date:                  2024-03-08T21:57:40.486099-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
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
