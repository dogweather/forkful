---
title:                "HTTP अनुरोध भेजना"
date:                  2024-03-08T21:56:40.211280-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

Dart में HTTP अनुरोध भेजना एक वेब सर्वर या API के साथ Dart अनुप्रयोग से संचार आरंभ करने की प्रक्रिया है। प्रोग्रामर वेब से डेटा प्राप्त करने, फॉर्म्स सबमिट करने, और RESTful सेवाओं के साथ संवाद करने के लिए इसे करते हैं, जिससे यह Dart में वेब, सर्वर-साइड, और मोबाइल एप्लिकेशन विकास के लिए एक मूलभूत कार्यवाही बन जाती है।

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
