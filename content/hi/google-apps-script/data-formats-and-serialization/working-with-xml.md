---
changelog:
- 2024-02-01, gpt-4-0125-preview, translated from English
date: 2024-02-01 22:07:40.255042-07:00
description: "Google Apps Script \u092E\u0947\u0902 XML \u0915\u0947 \u0938\u093E\u0925\
  \ \u0915\u093E\u092E \u0915\u0930\u0928\u0947 \u0938\u0947 \u092A\u094D\u0930\u094B\
  \u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 XML \u0921\u0947\u091F\u093E \u0915\
  \u094B \u092A\u093E\u0930\u094D\u0938, \u0938\u0902\u0936\u094B\u0927\u093F\u0924\
  , \u0914\u0930 \u091C\u0928\u0930\u0947\u091F \u0915\u0930 \u0938\u0915\u0924\u0947\
  \ \u0939\u0948\u0902, \u091C\u094B \u0915\u093F \u0935\u0947\u092C \u0938\u0930\u094D\
  \u0935\u093F\u0938\u0947\u091C \u0914\u0930 \u0915\u0949\u0928\u094D\u092B\u093F\
  \u0917\u0930\u0947\u0936\u0928\u094D\u0938 \u0915\u0947 \u0932\u093F\u090F\u2026"
lastmod: '2024-03-13T22:44:51.558648-06:00'
model: gpt-4-0125-preview
summary: "Google Apps Script \u092E\u0947\u0902 XML \u0915\u0947 \u0938\u093E\u0925\
  \ \u0915\u093E\u092E \u0915\u0930\u0928\u0947 \u0938\u0947 \u092A\u094D\u0930\u094B\
  \u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 XML \u0921\u0947\u091F\u093E \u0915\
  \u094B \u092A\u093E\u0930\u094D\u0938, \u0938\u0902\u0936\u094B\u0927\u093F\u0924\
  , \u0914\u0930 \u091C\u0928\u0930\u0947\u091F \u0915\u0930 \u0938\u0915\u0924\u0947\
  \ \u0939\u0948\u0902, \u091C\u094B \u0915\u093F \u0935\u0947\u092C \u0938\u0930\u094D\
  \u0935\u093F\u0938\u0947\u091C \u0914\u0930 \u0915\u0949\u0928\u094D\u092B\u093F\
  \u0917\u0930\u0947\u0936\u0928\u094D\u0938 \u0915\u0947 \u0932\u093F\u090F \u0905\
  \u0928\u093F\u0935\u093E\u0930\u094D\u092F \u0939\u0948\u0964 \u092A\u094D\u0930\
  \u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0907\u0938 \u0924\u0930\
  \u0940\u0915\u0947 \u0915\u094B \u092A\u0941\u0930\u093E\u0928\u0947 \u0938\u093F\
  \u0938\u094D\u091F\u092E\u094D\u0938 \u0915\u0947 \u0938\u093E\u0925 \u090F\u0915\
  \u0940\u0915\u0930\u0923 \u0915\u0930\u0928\u0947, \u0935\u0947\u092C \u0938\u094D\
  \u0915\u094D\u0930\u0948\u092A\u093F\u0902\u0917 \u0915\u0930\u0928\u0947, \u092F\
  \u093E \u0909\u0928 \u0905\u0928\u0947\u0915 APIs \u0915\u0947 \u0938\u093E\u0925\
  \ \u0938\u0902\u091A\u093E\u0930 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\
  \u090F \u0905\u092A\u0928\u093E\u0924\u0947 \u0939\u0948\u0902 \u091C\u094B \u0921\
  \u0947\u091F\u093E \u0907\u0902\u091F\u0930\u091A\u0947\u0902\u091C \u0915\u0947\
  \ \u0932\u093F\u090F JSON \u0915\u0947 \u090A\u092A\u0930 \u0905\u092D\u0940 \u092D\
  \u0940 XML \u092A\u0930 \u0928\u093F\u0930\u094D\u092D\u0930 \u0915\u0930\u0924\u0947\
  \ \u0939\u0948\u0902\u0964."
title: "XML \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\u093E"
weight: 40
---

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
