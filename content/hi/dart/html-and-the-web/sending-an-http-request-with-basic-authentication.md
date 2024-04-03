---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:56:55.086846-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: \u0921\u093E\u0930\
  \u094D\u091F \u092E\u0947\u0902, \u0906\u092A `http` \u092A\u0948\u0915\u0947\u091C\
  \ \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\u0947 \u092E\u0942\
  \u0932 \u092A\u094D\u0930\u092E\u093E\u0923\u0940\u0915\u0930\u0923 \u0915\u0947\
  \ \u0938\u093E\u0925 HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\
  \ \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\u0964 \u092A\u0939\u0932\u0947, \u0905\
  \u092A\u0928\u0940 `pubspec.yaml` \u092B\u093E\u0907\u0932 \u092E\u0947\u0902 `http`\
  \ \u092A\u0948\u0915\u0947\u091C \u091C\u094B\u0921\u093C\u0947\u0902."
lastmod: '2024-03-13T22:44:51.812925-06:00'
model: gpt-4-0125-preview
summary: "\u0921\u093E\u0930\u094D\u091F \u092E\u0947\u0902, \u0906\u092A `http` \u092A\
  \u0948\u0915\u0947\u091C \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\
  \u0915\u0947 \u092E\u0942\u0932 \u092A\u094D\u0930\u092E\u093E\u0923\u0940\u0915\
  \u0930\u0923 \u0915\u0947 \u0938\u093E\u0925 HTTP \u0905\u0928\u0941\u0930\u094B\
  \u0927 \u092D\u0947\u091C \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\u0964 \u092A\
  \u0939\u0932\u0947, \u0905\u092A\u0928\u0940 `pubspec.yaml` \u092B\u093E\u0907\u0932\
  \ \u092E\u0947\u0902 `http` \u092A\u0948\u0915\u0947\u091C \u091C\u094B\u0921\u093C\
  \u0947\u0902."
title: "\u092C\u0947\u0938\u093F\u0915 \u092A\u094D\u0930\u092E\u093E\u0923\u0928\
  \ \u0915\u0947 \u0938\u093E\u0925 HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\
  \u0947\u091C\u0928\u093E"
weight: 45
---

## कैसे करें:
डार्ट में, आप `http` पैकेज का उपयोग करके मूल प्रमाणीकरण के साथ HTTP अनुरोध भेज सकते हैं। पहले, अपनी `pubspec.yaml` फाइल में `http` पैकेज जोड़ें:

```yaml
dependencies:
  http: ^0.13.4
```

फिर, अपनी Dart फाइल में पैकेज को आयात करें:

```dart
import 'package:http/http.dart' as http;
import 'dart:convert';
```

मूल प्रमाणीकरण के साथ एक GET अनुरोध भेजने के लिए, आप निम्नलिखित कोड का उपयोग कर सकते हैं:

```dart
Future<void> fetchUserData() async {
  final username = 'yourUsername';
  final password = 'yourPassword';
  final credentials = base64Encode(utf8.encode('$username:$password'));
  final response = await http.get(
    Uri.parse('https://yourapi.com/userdata'),
    headers: {
      'Authorization': 'Basic $credentials',
    },
  );

  if (response.statusCode == 200) {
    print('उपयोगकर्ता का डाटा सफलतापूर्वक लाया गया!');
    print('प्रतिक्रिया शरीर: ${response.body}');
  } else {
    print('उपयोगकर्ता डाटा लाने में विफल रहा, स्थिति कोड के साथ: ${response.statusCode}');
  }
}
```

यह कोड 'https://yourapi.com/userdata' पर एक GET अनुरोध भेजता है जिसमें एक मूल प्रमाणीकरण हैडर होता है। उपयोगकर्ता नाम और पासवर्ड को base64 में एन्कोड किया गया है और 'Authorization' हैडर में मूल एक्सेस प्रमाणीकरण मानकों के अनुसार दिया गया है।

**नमूना आउटपुट:**

सफल अनुरोध पर और यदि सर्वर 200 का स्थिति कोड वापस करता है, आप देख सकते हैं:

```plaintext
उपयोगकर्ता का डाटा सफलतापूर्वक लाया गया!
प्रतिक्रिया शरीर: {"id":1, "name":"John Doe", "email":"john@example.com"}
```

यदि प्रमाणीकरण विफल होता है या कोई अन्य त्रुटि होती है, तो प्रतिक्रिया स्थिति कोड समस्या की पहचान करने में मदद करेगा।
