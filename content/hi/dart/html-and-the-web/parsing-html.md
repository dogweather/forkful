---
title:                "HTML विश्लेषण"
date:                  2024-03-08T21:55:38.100531-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
प्रोग्रामिंग में HTML पार्सिंग का मतलब है HTML दस्तावेज़ों से डेटा निकालना। प्रोग्रामर इसे वेब सामग्री के साथ बातचीत करने या सूचना निष्कर्षण, परीक्षण, या स्वचालन प्रयोजनों के लिए वेब सामग्री से डेटा खुरचने के लिए करते हैं, यहाँ तक कि जब आधिकारिक APIs उपलब्ध नहीं होते।

## कैसे करें:
Dart अपने कोर लाइब्रेरीज़ में HTML पार्सिंग के लिए निर्मित समर्थन प्रदान नहीं करता। हालांकि, आप `html` जैसे तृतीय-पक्ष पैकेज का उपयोग करके HTML दस्तावेज़ों को पार्स और नियंत्रित कर सकते हैं। 

पहले, अपनी `pubspec.yaml` फाइल में `html` पैकेज जोड़ें:

```yaml
dependencies:
  html: ^0.15.0
```

फिर, अपनी Dart फाइल में पैकेज को आयात करें:

```dart
import 'package:html/parser.dart' show parse;
import 'package:html/dom.dart';
```

यहाँ एक बुनियादी उदाहरण दिया गया है जिसमें HTML वाली एक स्ट्रिंग को पार्स करके डेटा निकाला जा रहा है:

```dart
void main() {
  var htmlDocument = """
  <html>
    <body>
      <h1>नमस्ते, Dart!</h1>
      <p>यह एक पैराग्राफ है एक नमूना HTML में</p>
    </body>
  </html>
  """;

  // HTML स्ट्रिंग पार्स करें
  Document document = parse(htmlDocument);

  // डेटा निकालना
  String title = document.querySelector('h1')?.text ?? "कोई शीर्षक नहीं मिला";
  String paragraph = document.querySelector('p')?.text ?? "कोई पैराग्राफ नहीं मिला";

  print('शीर्षक: $title');
  print('पैराग्राफ: $paragraph');
}
```

आउटपुट:

```
शीर्षक: नमस्ते, Dart!
पैराग्राफ: यह एक पैराग्राफ है एक नमूना HTML में
```

वास्तविक विश्व वेब पेजेज़ के साथ बातचीत करने के लिए, आप `html` पार्सिंग को HTTP अनुरोधों (वेब सामग्री लाने के लिए `http` पैकेज का उपयोग करके) के साथ जोड़ सकते हैं। यहाँ एक त्वरित उदाहरण दिया गया है:

सबसे पहले, `html` के साथ `http` पैकेज जोड़ें:

```yaml
dependencies:
  html: ^0.15.0
  http: ^0.13.3
```

फिर, वेब से एक HTML पेज लाकर पार्स करें:

```dart
import 'package:http/http.dart' as http;
import 'package:html/parser.dart' show parse;

void main() async {
  var url = 'https://example.com';
  
  // वेबपेज लाएं
  var response = await http.get(Uri.parse(url));
  
  यदि (response.statusCode == 200) {
    var document = parse(response.body);

    // मान लें कि पेज में <h1> टैग हैं जिनमें आपकी रुचि है
    var headlines = document.querySelectorAll('h1').map((e) => e.text).toList();
    
    print('समाचार शीर्षक: $headlines');
  } else {
    print('अनुरोध विफल हो गया स्थिति के साथ: ${response.statusCode}.');
  }
}
```

नोट: ऊपर दिखाई गई वेब स्क्रेपिंग तकनीक का उपयोग वेबसाइट की सेवा की शर्तों के अनुसार जिम्मेदारीपूर्वक किया जाना चाहिए।
