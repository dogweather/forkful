---
title:                "XML के साथ काम करना"
date:                  2024-03-08T21:57:48.704596-07:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-08, OpenAIModel.GPT_4_TURBO, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

Dart में XML के साथ काम करना XML दस्तावेजों के पार्सिंग, पूछताछ, और संशोधन की प्रक्रिया में शामिल है, जो वेब सेवाओं, कॉन्फ़िगरेशन फाइलों, या पुरानी सिस्टमों के साथ संवाद करने वाले एप्लिकेशनों के लिए महत्वपूर्ण है। प्रोग्रामर ऐसा करते हैं ताकि डेटा आदान-प्रदान, कॉन्फ़िगरेशन, या यहां तक कि रिमोट प्रोसीजर कॉल्स को एक संरचित, पदानुक्रमिक प्रारूप में सक्षम कर सकें जो दोनों मानव-पठनीय और मशीन-पार्सयोग्य है।

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
