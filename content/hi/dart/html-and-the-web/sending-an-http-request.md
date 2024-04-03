---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:40.211280-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: Dart \u092E\u0947\
  \u0902 `http` \u092A\u0948\u0915\u0947\u091C \u0936\u093E\u092E\u093F\u0932 \u0939\
  \u0948, \u091C\u094B HTTP \u0938\u0902\u0938\u093E\u0927\u0928\u094B\u0902 \u0915\
  \u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\u0947 \u0915\u093E\
  \ \u090F\u0915 \u0936\u0915\u094D\u0924\u093F\u0936\u093E\u0932\u0940 \u0914\u0930\
  \ \u0938\u0941\u0935\u093F\u0927\u093E\u091C\u0928\u0915 \u0924\u0930\u0940\u0915\
  \u093E \u0939\u0948\u0964 \u092A\u0939\u0932\u0947, \u0907\u0938\u0947 \u0905\u092A\
  \u0928\u0940 pubspec.yaml \u092B\u093E\u0907\u0932 \u092E\u0947\u0902\u2026"
lastmod: '2024-03-13T22:44:51.807507-06:00'
model: gpt-4-0125-preview
summary: "Dart \u092E\u0947\u0902 `http` \u092A\u0948\u0915\u0947\u091C \u0936\u093E\
  \u092E\u093F\u0932 \u0939\u0948, \u091C\u094B HTTP \u0938\u0902\u0938\u093E\u0927\
  \u0928\u094B\u0902 \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\
  \u0928\u0947 \u0915\u093E \u090F\u0915 \u0936\u0915\u094D\u0924\u093F\u0936\u093E\
  \u0932\u0940 \u0914\u0930 \u0938\u0941\u0935\u093F\u0927\u093E\u091C\u0928\u0915\
  \ \u0924\u0930\u0940\u0915\u093E \u0939\u0948\u0964 \u092A\u0939\u0932\u0947, \u0907\
  \u0938\u0947 \u0905\u092A\u0928\u0940 pubspec.yaml \u092B\u093E\u0907\u0932 \u092E\
  \u0947\u0902 \u0936\u093E\u092E\u093F\u0932 \u0915\u0930\u0947\u0902."
title: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u0928\u093E"
weight: 44
---

## कैसे करें:
Dart में `http` पैकेज शामिल है, जो HTTP संसाधनों के साथ काम करने का एक शक्तिशाली और सुविधाजनक तरीका है। पहले, इसे अपनी pubspec.yaml फाइल में शामिल करें:

```yaml
dependencies:
  http: ^0.13.3
```

फिर, इसे अपने Dart कोड में आयात करके अनुरोध करना शुरू करें:

```dart
import 'package:http/http.dart' as http;

void main() async {
  var url = Uri.parse('https://jsonplaceholder.typicode.com/todos/1');
  var response = await http.get(url);

  यदि (response.statusCode == 200) {
    print('प्रतिक्रिया शरीर: ${response.body}');
  } else {
    print('अनुरोध विफल रहा, स्थिति: ${response.statusCode}.');
  }
}
```

सफल अनुरोध के लिए नमूना आउटपुट इस तरह दिख सकता है:

```
प्रतिक्रिया शरीर: {
  "userId": 1,
  "id": 1,
  "title": "delectus aut autem",
  "completed": false
}
```

अधिक जटिल अनुरोधों के लिए, जैसे कि JSON बॉडी के साथ POST अनुरोध, आप निम्नलिखित कार्य करेंगे:

```dart
import 'dart:convert';
import 'package:http/http.dart' as http;

void main() async {
  var url = Uri.parse('https://jsonplaceholder.typicode.com/posts');
  var response = await http.post(
    url,
    headers: {"Content-Type": "application/json"},
    body: jsonEncode({
      "title": 'foo',
      "body": 'bar',
      "userId": 1,
    }),
  );

  यदि (response.statusCode == 201) {
    print('प्रतिक्रिया स्थिति: ${response.statusCode}');
    print('प्रतिक्रिया शरीर: ${response.body}');
  } else {
    print('नई पोस्ट बनाने में विफल। स्थिति: ${response.statusCode}');
  }
}
```

POST अनुरोध के लिए नमूना आउटपुट हो सकता है:

```
प्रतिक्रिया स्थिति: 201
प्रतिक्रिया शरीर: {
  "title": "foo",
  "body": "bar",
  "userId": 1,
  "id": 101
}
```

ये उदाहरण Dart में `http` पैकेज का उपयोग करके बुनियादी HTTP GET और POST अनुरोध दिखाते हैं। यह पैकेज हेडर्स और बॉडी कंटेंट के साथ अधिक जटिल परिदृश्यों सहित HTTP अनुरोध भेजने की अधिकांश आवश्यकताओं को कवर करता है।
