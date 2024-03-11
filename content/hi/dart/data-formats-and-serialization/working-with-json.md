---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:58:26.009439-07:00
description: "JSON (\u091C\u093E\u0935\u093E\u0938\u094D\u0915\u094D\u0930\u093F\u092A\
  \u094D\u091F \u0911\u092C\u094D\u091C\u0947\u0915\u094D\u091F \u0928\u094B\u091F\
  \u0947\u0936\u0928) \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u0930\u094D\u092F\
  \ \u0915\u0930\u0928\u093E \u0921\u093E\u0930\u094D\u091F \u0911\u092C\u094D\u091C\
  \u0947\u0915\u094D\u091F\u094D\u0938 \u092E\u0947\u0902 \u0938\u094D\u091F\u094D\
  \u0930\u093F\u0902\u0917\u094D\u0938 \u0938\u0947 JSON \u0921\u0947\u091F\u093E\
  \ \u092A\u093E\u0930\u094D\u0938 \u0915\u0930\u0928\u0947 \u0914\u0930 \u0907\u0938\
  \u0915\u0947 \u0935\u093F\u092A\u0930\u0940\u0924 \u0915\u093E \u0915\u093E\u0930\
  \u094D\u092F \u0936\u093E\u092E\u093F\u0932 \u0939\u0948, \u091C\u094B \u0915\u093F\
  \ \u0935\u0947\u092C\u2026"
lastmod: '2024-03-11T00:14:25.730716-06:00'
model: gpt-4-0125-preview
summary: "JSON (\u091C\u093E\u0935\u093E\u0938\u094D\u0915\u094D\u0930\u093F\u092A\
  \u094D\u091F \u0911\u092C\u094D\u091C\u0947\u0915\u094D\u091F \u0928\u094B\u091F\
  \u0947\u0936\u0928) \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u0930\u094D\u092F\
  \ \u0915\u0930\u0928\u093E \u0921\u093E\u0930\u094D\u091F \u0911\u092C\u094D\u091C\
  \u0947\u0915\u094D\u091F\u094D\u0938 \u092E\u0947\u0902 \u0938\u094D\u091F\u094D\
  \u0930\u093F\u0902\u0917\u094D\u0938 \u0938\u0947 JSON \u0921\u0947\u091F\u093E\
  \ \u092A\u093E\u0930\u094D\u0938 \u0915\u0930\u0928\u0947 \u0914\u0930 \u0907\u0938\
  \u0915\u0947 \u0935\u093F\u092A\u0930\u0940\u0924 \u0915\u093E \u0915\u093E\u0930\
  \u094D\u092F \u0936\u093E\u092E\u093F\u0932 \u0939\u0948, \u091C\u094B \u0915\u093F\
  \ \u0935\u0947\u092C\u2026"
title: "JSON \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\
  \u093E"
---

{{< edit_this_page >}}

## क्या और क्यों?

JSON (जावास्क्रिप्ट ऑब्जेक्ट नोटेशन) के साथ कार्य करना डार्ट ऑब्जेक्ट्स में स्ट्रिंग्स से JSON डेटा पार्स करने और इसके विपरीत का कार्य शामिल है, जो कि वेब और ऐप विकास में डेटा इंटरचेंज के लिए एक सामान्य कार्य है। प्रोग्रामर्स इसे एपीआई, कॉन्फिगरेशन, या उनके ऐप्स के अंदर घटकों के बीच संवाद के डेटा को कुशलतापूर्वक संभालने के लिए करते हैं।

## कैसे करें:

डार्ट `dart:convert` लाइब्रेरी के साथ JSON के लिए निर्मित सहायता प्रदान करता है, जिससे JSON को एनकोड और डिकोड करना सीधा है। नीचे बुनियादी कार्यों को दर्शाने वाले उदाहरण दिए गए हैं:

**JSON स्ट्रिंग को डार्ट ऑब्जेक्ट में पार्स करना:**
```dart
import 'dart:convert';

void main() {
  // उदाहरण JSON स्ट्रिंग
  String jsonString = '{"name": "John", "age": 30, "email": "john@example.com"}';
  
  // JSON को डार्ट मैप में डिकोड करना
  Map<String, dynamic> user = jsonDecode(jsonString);
  
  print('Hello, ${user['name']}! आप ${user['age']} वर्ष के हैं।');
  // आउटपुट: Hello, John! आप 30 वर्ष के हैं।
}
```

**डार्ट ऑब्जेक्ट को JSON स्ट्रिंग में एनकोड करना:**
```dart
import 'dart:convert';

void main() {
  // उदाहरण डार्ट ऑब्जेक्ट
  Map<String, dynamic> user = {
    'name': 'Jane',
    'age': 25,
    'email': 'jane@example.com'
  };
  
  // डार्ट मैप को JSON में एनकोडिंग
  String jsonString = jsonEncode(user);
  
  print(jsonString);
  // आउटपुट: {"name":"Jane","age":25,"email":"jane@example.com"}
}
```

**जटिल मॉडल्स के लिए `json_serializable` का उपयोग करना:**
जटिल डेटा मॉडल्स के लिए, मैन्युअल सीरियलाइजेशन बहुत थकाऊ हो सकता है। `json_serializable` पैकेज इस प्रक्रिया को स्वचालित करता है। इसके लिए अतिरिक्त सेटअप की आवश्यकता होती है, जिसमें आपके `pubspec.yaml` में निर्भरताएँ जोड़ना और बिल्ड फाइलें बनाना शामिल है। सेटअप के बाद, आप इसे निम्नलिखित तरीके से उपयोग कर सकते हैं:

1. एनोटेशन के साथ एक मॉडल परिभाषित करें:
```dart
import 'package:json_annotation/json_annotation.dart';

part 'user.g.dart';

@JsonSerializable()
class User {
  String name;
  int age;
  String email;
  
  User({required this.name, required this.age, required this.email});
  
  factory User.fromJson(Map<String, dynamic> json) => _$UserFromJson(json);
  Map<String, dynamic> toJson() => _$UserToJson(this);
}
```

2. धारावाहिकीकरण बॉयलरप्लेट उत्पन्न करें:
`user.g.dart` फाइल उत्पन्न करने के लिए बिल्ड रनर कमांड का उपयोग करें:
```shell
flutter pub run build_runner build
```

3. अपने मॉडल का उपयोग करें:
```dart
void main() {
  // User को JSON में पार्स करना
  Map userMap = jsonDecode('{"name": "John", "age": 30, "email": "john@example.com"}');
  User user = User.fromJson(userMap);
  
  print('User: ${user.name}, Age: ${user.age}');
  // आउटपुट: User: John, Age: 30

  // User को वापस JSON में परिवर्तित करना
  String jsonString = jsonEncode(user.toJson());
  print(jsonString);
  // आउटपुट: {"name":"John","age":30,"email":"john@example.com"}
}
```

ये उदाहरण डार्ट में बुनियादी और उन्नत JSON सहभागिता को दर्शाते हैं, विकासकर्ताओं को उनके एप्लिकेशन में डेटा धारावाहिकीकरण कार्यों को सहजतापूर्वक संभालने के लिए सशक्त बनाते हैं।
