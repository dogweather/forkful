---
changelog:
- 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
date: 2024-03-08 21:57:48.704596-07:00
description: "\u0915\u0948\u0938\u0947: Dart \u0905\u092A\u0928\u0947 \u0938\u094D\
  \u091F\u0948\u0902\u0921\u0930\u094D\u0921 \u0932\u093E\u0907\u092C\u094D\u0930\u0947\
  \u0930\u0940 \u092E\u0947\u0902 XML \u0939\u0948\u0902\u0921\u0932\u093F\u0902\u0917\
  \ \u0915\u0947 \u0932\u093F\u090F \u092C\u093F\u0932\u094D\u091F-\u0907\u0928 \u0938\
  \u092A\u094B\u0930\u094D\u091F \u0936\u093E\u092E\u093F\u0932 \u0928\u0939\u0940\
  \u0902 \u0915\u0930\u0924\u093E \u0939\u0948, \u091C\u093F\u0938\u0938\u0947 \u0924\
  \u0943\u0924\u0940\u092F-\u092A\u0915\u094D\u0937 \u092A\u0948\u0915\u0947\u091C\
  \u094B\u0902 \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0906\u0935\u0936\u094D\
  \u092F\u0915 \u0939\u094B \u091C\u093E\u0924\u093E \u0939\u0948\u0964 \u090F\u0915\
  \u2026"
lastmod: '2024-03-13T22:44:51.857877-06:00'
model: gpt-4-0125-preview
summary: "Dart \u0905\u092A\u0928\u0947 \u0938\u094D\u091F\u0948\u0902\u0921\u0930\
  \u094D\u0921 \u0932\u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940 \u092E\u0947\
  \u0902 XML \u0939\u0948\u0902\u0921\u0932\u093F\u0902\u0917 \u0915\u0947 \u0932\u093F\
  \u090F \u092C\u093F\u0932\u094D\u091F-\u0907\u0928 \u0938\u092A\u094B\u0930\u094D\
  \u091F \u0936\u093E\u092E\u093F\u0932 \u0928\u0939\u0940\u0902 \u0915\u0930\u0924\
  \u093E \u0939\u0948, \u091C\u093F\u0938\u0938\u0947 \u0924\u0943\u0924\u0940\u092F\
  -\u092A\u0915\u094D\u0937 \u092A\u0948\u0915\u0947\u091C\u094B\u0902 \u0915\u093E\
  \ \u0909\u092A\u092F\u094B\u0917 \u0906\u0935\u0936\u094D\u092F\u0915 \u0939\u094B\
  \ \u091C\u093E\u0924\u093E \u0939\u0948\u0964 \u090F\u0915 \u0932\u094B\u0915\u092A\
  \u094D\u0930\u093F\u092F \u092A\u0948\u0915\u0947\u091C `xml` \u0939\u0948\u0964\
  \ \u0907\u0938\u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0928\u0947\
  \ \u0915\u0947 \u0932\u093F\u090F, \u0906\u092A\u0915\u094B \u0938\u092C\u0938\u0947\
  \ \u092A\u0939\u0932\u0947 \u0907\u0938\u0947 \u0905\u092A\u0928\u0947 `pubspec.yaml`\
  \ \u092E\u0947\u0902 \u091C\u094B\u0921\u093C\u0928\u093E \u092A\u0921\u093C\u0947\
  \u0917\u093E."
title: "XML \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\u093E"
weight: 40
---

## कैसे:
Dart अपने स्टैंडर्ड लाइब्रेरी में XML हैंडलिंग के लिए बिल्ट-इन सपोर्ट शामिल नहीं करता है, जिससे तृतीय-पक्ष पैकेजों का उपयोग आवश्यक हो जाता है। एक लोकप्रिय पैकेज `xml` है। इसका उपयोग करने के लिए, आपको सबसे पहले इसे अपने `pubspec.yaml` में जोड़ना पड़ेगा:

```yaml
dependencies:
  xml: ^5.0.0 // उपलब्ध नवीनतम संस्करण का उपयोग करें
```

फिर, अपनी Dart फ़ाइल में पैकेज को इम्पोर्ट करें:

```dart
import 'package:xml/xml.dart' as xml;
```

**XML पार्सिंग:**

मान लीजिए आपके पास एक XML स्ट्रिंग इस तरह है:

```xml
<String name="greeting">Hello, world!</String>
```

आप XML को निम्न प्रकार पढ़ और पार्स कर सकते हैं:

```dart
void parseXml(String xmlString) {
    final document = xml.XmlDocument.parse(xmlString);
    final String content = document.findElements('String').single.getAttribute('name');
    print(content); // Outputs: greeting
}

void main() {
  final xmlString = '<String name="greeting">Hello, world!</String>';
  parseXml(xmlString);
}
```

**XML दस्तावेज़ बनाना:**

`xml` पैकेज के साथ नया XML दस्तावेज़ बनाना सरल है:

```dart
void createXml() {
  final builder = xml.XmlBuilder();
  builder.processing('xml', 'version="1.0"');
  builder.element('greeting', nest: () {
    builder.attribute('name', 'hello');
    builder.text('Hello, world!');
  });
  final xmlDocument = builder.buildDocument();
  print(xmlDocument.toXmlString(pretty: true));
}

void main() {
  createXml();
}
```

**आउटपुट**:

```xml
<?xml version="1.0"?>
<greeting name="hello">Hello, world!</greeting>
```

**XML पूछताछ और संशोधन:**

एलिमेंट्स को खोजने या संशोधित करने के लिए, आप XPath-जैसे तरीकों का उपयोग कर सकते हैं:

```dart
void modifyXml(String xmlString) {
    var document = xml.XmlDocument.parse(xmlString);
    var greeting = document.findAllElements('greeting').first;
    
    // 'name' विशेषता का संशोधन
    greeting.setAttribute('name', 'greeting_modified');
    
    // एक नया चाइल्ड एलिमेंट जोड़ना
    greeting.children.add(xml.XmlElement(xml.XmlName('message'), [], [xml.XmlText('Goodbye!')]));
    
    print(document.toXmlString(pretty: true));
}

void main() {
  final xmlString = '<greeting name="hello">Hello, world!</greeting>';
  modifyXml(xmlString);
}
```

**आउटपुट**:

```xml
<greeting name="greeting_modified">
  Hello, world!
  <message>Goodbye!</message>
</greeting>
```

ये उदाहरण Dart में XML के साथ काम करने के लिए बुनियादी कार्यों का प्रदर्शन करते हैं। `xml` पैकेज के साथ, आप अपने एप्लिकेशन आवश्यकताओं को पूरा करने के लिए XML दस्तावेज़ों को पार्स, बनाने, और प्रबंधित कर सकते हैं।
