---
date: 2024-01-26 04:33:44.729635-07:00
description: "XML \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\
  \u093E \u0915\u093E \u092E\u0924\u0932\u092C \u0939\u0948 \u0915\u094B\u0921 \u0915\
  \u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\u0947 XML \u0938\u093E\u092E\
  \u0917\u094D\u0930\u0940 \u0915\u093E \u092A\u093E\u0930\u094D\u0938 \u0915\u0930\
  \u0928\u093E, \u0939\u0947\u0930\u092B\u0947\u0930 \u0915\u0930\u0928\u093E \u0914\
  \u0930 \u0909\u0924\u094D\u092A\u093E\u0926\u0928 \u0915\u0930\u0928\u093E\u0964\
  \ \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930 \u0907\u0938\u0947\
  \ \u0915\u0930\u0924\u0947 \u0939\u0948\u0902 \u0915\u094D\u092F\u094B\u0902\u0915\
  \u093F XML, \u0907\u0938\u0915\u0947 \u092E\u093E\u0928\u0935-\u2026"
lastmod: '2024-03-13T22:44:53.031704-06:00'
model: gpt-4-0125-preview
summary: "XML \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\
  \u093E \u0915\u093E \u092E\u0924\u0932\u092C \u0939\u0948 \u0915\u094B\u0921 \u0915\
  \u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\u0947 XML \u0938\u093E\u092E\
  \u0917\u094D\u0930\u0940 \u0915\u093E \u092A\u093E\u0930\u094D\u0938 \u0915\u0930\
  \u0928\u093E, \u0939\u0947\u0930\u092B\u0947\u0930 \u0915\u0930\u0928\u093E \u0914\
  \u0930 \u0909\u0924\u094D\u092A\u093E\u0926\u0928 \u0915\u0930\u0928\u093E\u0964\
  \ \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930 \u0907\u0938\u0947\
  \ \u0915\u0930\u0924\u0947 \u0939\u0948\u0902 \u0915\u094D\u092F\u094B\u0902\u0915\
  \u093F XML, \u0907\u0938\u0915\u0947 \u092E\u093E\u0928\u0935-\u092A\u0920\u0928\
  \u0940\u092F \u0914\u0930 \u092E\u0936\u0940\u0928-\u092A\u093E\u0930\u094D\u0938\
  \u092F\u094B\u0917\u094D\u092F \u0938\u094D\u0935\u092D\u093E\u0935 \u0915\u0947\
  \ \u0915\u093E\u0930\u0923, \u0915\u0949\u0928\u094D\u092B\u093C\u093F\u0917\u0930\
  \u0947\u0936\u0928 \u092B\u093C\u093E\u0907\u0932, \u0921\u0947\u091F\u093E \u090F\
  \u0915\u094D\u0938\u091A\u0947\u0902\u091C, \u0914\u0930 \u0935\u0947\u092C \u0938\
  \u0947\u0935\u093E\u0913\u0902 \u0915\u0947 \u0932\u093F\u090F \u0935\u094D\u092F\
  \u093E\u092A\u0915 \u0930\u0942\u092A \u0938\u0947 \u0909\u092A\u092F\u094B\u0917\
  \ \u0915\u093F\u092F\u093E \u091C\u093E\u0924\u093E \u0939\u0948\u0964."
title: "XML \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\u093E"
weight: 40
---

## कैसे करें:
यहाँ पर XML को पार्स कैसे करें बताया गया है:

```javascript
let parser = new DOMParser();
let xmlString = `<note>
                    <to>User</to>
                    <from>Author</from>
                    <heading>Reminder</heading>
                    <body>इस सप्ताहांत मुझे मत भूलना!</body>
                 </note>`;

let xmlDoc = parser.parseFromString(xmlString, "application/xml");
console.log(xmlDoc.getElementsByTagName('to')[0].childNodes[0].nodeValue);
// आउटपुट: User
```

और XML उत्पादित करने के लिए:

```javascript
let xmlDocument = document.implementation.createDocument('', '', null);
let noteElement = xmlDocument.createElement('note');
noteElement.appendChild(xmlDocument.createElement('to')).textContent = 'User';
xmlDocument.appendChild(noteElement);
let serializer = new XMLSerializer();
let xmlString = serializer.serializeToString(xmlDocument);
console.log(xmlString);
// आउटपुट: <note><to>User</to></note>
```

## गहराई से जाने
XML, एक्सटेंसिबल मार्कअप लैंग्वेज के लिए छोटा है, एक डेटा प्रारूप है जो 90 के दशक के अंत से इस्तेमाल में है। यह मानवों और मशीनों दोनों द्वारा पढ़े जा सकने वाले दस्तावेजों को एनकोड करने के लिए नियमों का एक समूह परिभाषित करता है। ऐतिहासिक रूप से, XML इसकी लचीलापन और संरचित पदानुक्रम के लिए प्रसिद्ध हुआ, जिससे यह SOAP जैसी वेब सेवाओं, और अनेक कॉन्फ़िगरेशन फ़ाइलों के लिए एक विकल्प बन गया।

XML के विकल्पों में JSON (JavaScript ऑब्जेक्ट नोटेशन) शामिल है, जो JavaScript के साथ इसके उपयोग में आसानी और हल्के वजन के कारण लोकप्रिय हो गया है। YAML एक और विकल्प है, जिसे मानव-अनुकूल होने और कॉन्फ़िगरेशन के लिए एक सामान्य विकल्प होने के लिए मूल्यांकित किया जाता है।

JavaScript में XML का कार्यान्वयन DOMParser और XMLSerializer इंटरफेस का उपयोग करके किया जाता है। XML DOM (डॉक्यूमेंट ऑब्जेक्ट मॉडल) HTML के साथ जैसे आप XML दस्तावेजों को नेविगेट और संपादित कर सकते हैं उसकी अनुमति देता है। JSON के उदय के बावजूद, XML की समझ महत्वपूर्ण है, क्योंकि अनेक पुराने सिस्टम और विशेष उद्योग अभी भी डेटा एक्सचेंज के लिए इस पर निर्भर करते हैं।

## देखें भी
- MDN वेब डॉक्स (XML पार्सिंग): https://developer.mozilla.org/en-US/docs/Web/API/DOMParser
- W3Schools (XML DOM ट्यूटोरियल): https://www.w3schools.com/xml/dom_intro.asp
- "XML क्या है?": https://www.w3.org/XML/
