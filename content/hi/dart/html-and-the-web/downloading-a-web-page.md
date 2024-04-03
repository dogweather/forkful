---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:54:43.295222-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: Dart \u090F\u0915\
  \ \u0932\u094B\u0915\u092A\u094D\u0930\u093F\u092F \u0924\u0943\u0924\u0940\u092F\
  -\u092A\u0915\u094D\u0937 \u092A\u0941\u0938\u094D\u0924\u0915\u093E\u0932\u092F\
  \ `http` \u092A\u0948\u0915\u0947\u091C \u092A\u094D\u0930\u0926\u093E\u0928 \u0915\
  \u0930\u0924\u093E \u0939\u0948, \u091C\u094B HTTP \u0905\u0928\u0941\u0930\u094B\
  \u0927\u094B\u0902 \u0915\u094B \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\
  \u090F \u0939\u0948\u0964 \u092F\u0939\u093E\u0902 \u090F\u0915 \u0935\u0947\u092C\
  \u092A\u0947\u091C \u0915\u094B \u0921\u093E\u0909\u0928\u0932\u094B\u0921 \u0915\
  \u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F\u2026"
lastmod: '2024-03-13T22:44:51.811131-06:00'
model: gpt-4-0125-preview
summary: "Dart \u090F\u0915 \u0932\u094B\u0915\u092A\u094D\u0930\u093F\u092F \u0924\
  \u0943\u0924\u0940\u092F-\u092A\u0915\u094D\u0937 \u092A\u0941\u0938\u094D\u0924\
  \u0915\u093E\u0932\u092F `http` \u092A\u0948\u0915\u0947\u091C \u092A\u094D\u0930\
  \u0926\u093E\u0928 \u0915\u0930\u0924\u093E \u0939\u0948, \u091C\u094B HTTP \u0905\
  \u0928\u0941\u0930\u094B\u0927\u094B\u0902 \u0915\u094B \u0915\u0930\u0928\u0947\
  \ \u0915\u0947 \u0932\u093F\u090F \u0939\u0948\u0964 \u092F\u0939\u093E\u0902 \u090F\
  \u0915 \u0935\u0947\u092C\u092A\u0947\u091C \u0915\u094B \u0921\u093E\u0909\u0928\
  \u0932\u094B\u0921 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u0907\
  \u0938\u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0948\u0938\u0947 \u0915\
  \u0930\u0947\u0902, \u0915\u093E \u090F\u0915 \u092E\u094C\u0932\u093F\u0915 \u0909\
  \u0926\u093E\u0939\u0930\u0923 \u0926\u093F\u092F\u093E \u0917\u092F\u093E \u0939\
  \u0948."
title: "\u0935\u0947\u092C \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\u094B\
  \u0921 \u0915\u0930\u0928\u093E"
weight: 42
---

## कैसे करें:
Dart एक लोकप्रिय तृतीय-पक्ष पुस्तकालय `http` पैकेज प्रदान करता है, जो HTTP अनुरोधों को करने के लिए है। यहां एक वेबपेज को डाउनलोड करने के लिए इसका उपयोग कैसे करें, का एक मौलिक उदाहरण दिया गया है:

सबसे पहले, अपनी `pubspec.yaml` में `http` पैकेज जोड़ें:
```
dependencies:
  http: ^0.13.3
```

फिर, पैकेज को इम्पोर्ट करें और इसका उपयोग एक वेब पेज की सामग्री को लाने के लिए करें:

```dart
import 'package:http/http.dart' as http;

Future<void> main() async {
  var url = Uri.parse('http://example.com');
  var response = await http.get(url);
  if (response.statusCode == 200) {
    print('पेज डाउनलोड हुआ:');
    print(response.body);
  } else {
    print('अनुरोध विफल रहा, स्थिति: ${response.statusCode}.');
  }
}
```

**नमूना आउटपुट** (यह वेब पेज की सामग्री के आधार पर भिन्न होगा):
```
पेज डाउनलोड हुआ:
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```

कूकीज़ को संभालने या यूज़र-एजेंट हेडर्स सेट करने जैसे अधिक जटिल परिस्थितियों के लिए, आप अपने अनुरोध में अतिरिक्त कॉन्फ़िगरेशन के साथ उसी `http` पैकेज का उपयोग करेंगे:
```dart
import 'package:http/http.dart' as http;

Future<void> main() async {
  var headers = {
    'User-Agent': 'YourCustomUserAgent/1.0',
    'Cookie': 'name=value; name2=value2',
  };
  var url = Uri.parse('http://example.com');
  var response = await http.get(url, headers: headers);

  if (response.statusCode == 200) {
    print('पेज कस्टम हेडर्स के साथ डाउनलोड हुआ:');
    print(response.body);
  } else {
    print('अनुरोध विफल रहा, स्थिति: ${response.statusCode}.');
  }
}
```

इस तरह के हेडर्स का उपयोग करके आप ब्राउज़र अनुरोधों की अधिक सटीक नकल कर सकते हैं, जो विशेष आवश्यकताओं या स्क्रेपिंग के खिलाफ सुरक्षाओं वाली साइटों से निपटने में विशेष रूप से उपयोगी है।
