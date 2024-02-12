---
title:                "XML के साथ काम करना"
aliases:
- hi/google-apps-script/working-with-xml.md
date:                  2024-02-01T22:07:40.255042-07:00
model:                 gpt-4-0125-preview
simple_title:         "XML के साथ काम करना"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/google-apps-script/working-with-xml.md"
changelog:
  - 2024-02-01, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

Google Apps Script में XML के साथ काम करने से प्रोग्रामर्स XML डेटा को पार्स, संशोधित, और जनरेट कर सकते हैं, जो कि वेब सर्विसेज और कॉन्फिगरेशन्स के लिए अनिवार्य है। प्रोग्रामर्स इस तरीके को पुराने सिस्टम्स के साथ एकीकरण करने, वेब स्क्रैपिंग करने, या उन अनेक APIs के साथ संचार करने के लिए अपनाते हैं जो डेटा इंटरचेंज के लिए JSON के ऊपर अभी भी XML पर निर्भर करते हैं।

## कैसे:

Google Apps Script, XML डेटा के साथ काम करने के लिए `XmlService` प्रदान करता है। नीचे हम एक XML स्ट्रिंग को पार्स करने, उसकी सामग्री को संशोधित करने, और एक नया XML स्ट्रिंग जनरेट करने का तरीका दर्शाते हैं।

XML स्ट्रिंग पार्स करना:

```javascript
function parseXML() {
  var xmlString = '<root><child name="first">Hello</child><child name="second">World</child></root>';
  var document = XmlService.parse(xmlString);
  var root = document.getRootElement();
  var children = root.getChildren('child');
  Logger.log(children[0].getText()); // लॉग: Hello
}
```

XML में संशोधन के लिए, आप एक नया बच्चा तत्व जोड़ना चाहेंगे:

```javascript
function addNewChild() {
  var xmlString = '<root><child name="first">Hello</child></root>';
  var document = XmlService.parse(xmlString);
  var root = document.getRootElement();
  
  var newChild = XmlService.createElement('child').setText('World');
  root.addContent(newChild);
  
  var xml = XmlService.getPrettyFormat().format(document);
  Logger.log(xml);
  // नए जोड़े गए बच्चे तत्व के साथ नया XML स्ट्रिंग लॉग करता है
}
```

खरोंच से XML स्ट्रिंग जनरेट करना:

```javascript
function createXML() {
  var root = XmlService.createElement('root');
  var child = XmlService.createElement('child').setText('Hello World');
  root.addContent(child);
  
  var xml = XmlService.getPrettyFormat().format(XmlService.createDocument(root));
  Logger.log(xml);
  // आउटपुट्स: <root><child>Hello World</child></root>
}
```

## गहराई में जानकारी

ऐतिहासिक रूप से, XML (Extensible Markup Language) डेटा इंटरचेंज के लिए एक मानक था जब तक कि JSON एक हल्के विकल्प के रूप में प्रकट नहीं हुआ। XML का विस्तारपूर्वक सिंटैक्स और सख्त पार्सिंग मॉडल एक मजबूत, हालांकि भारी, डेटा फॉर्मेट प्रदान करता था। Google Apps Script में, `XmlService` API XML डेटा का निर्माण, पार्सिंग, और संशोधन एनकैप्सुलेट करता है, विभिन्न लिगेसी और एंटरप्राइज सिस्टम्स, SOAP वेब सर्विसेज, और एप्लिकेशन्स के लिए कॉन्फिगरेशन फाइलों में इसकी जारी महत्वता को स्वीकार करता है।

वर्तमान वेब विकास में JSON की प्रचलितता के बावजूद, जो इसकी सादगी और JavaScript के साथ उपयोग में आसानी के लिए जाना जाता है, XML उन क्षेत्रों में प्रासंगिक बना हुआ है जहां दस्तावेज़ सत्यापन और संरचित पदानुक्रम महत्वपूर्ण हैं। हालांकि, नई परियोजनाओं के लिए, विशेषकर वेब APIs की ओर झुकाव रखने वालों के लिए, JSON अक्सर वजन में हल्के और JavaScript के साथ सहज एकीकरण के कारण अधिक व्यावहारिक विकल्प होता है।

Google Apps Script में XML को समझना और इसका प्रबंधन पर्यावरणों में काम करने वाले डेवलपर्स के लिए परम आवश्यक है जहां पुराने सिस्टमों या विशिष्ट एंटरप्राइज APIs के साथ एकीकरण आवश्यक है। हालांकि, नई परियोजनाओं को शुरू करते समय या जब लचीलापन महत्वपूर्ण हो, तो JSON जैसे विकल्पों के ऊपर XML की आवश्यकता का मूल्यांकन करना सलाह दी जाती है।
