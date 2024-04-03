---
date: 2024-01-26 04:33:44.729635-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: \u092F\u0939\u093E\
  \u0901 \u092A\u0930 XML \u0915\u094B \u092A\u093E\u0930\u094D\u0938 \u0915\u0948\
  \u0938\u0947 \u0915\u0930\u0947\u0902 \u092C\u0924\u093E\u092F\u093E \u0917\u092F\
  \u093E \u0939\u0948."
lastmod: '2024-03-13T22:44:53.031704-06:00'
model: gpt-4-0125-preview
summary: "\u092F\u0939\u093E\u0901 \u092A\u0930 XML \u0915\u094B \u092A\u093E\u0930\
  \u094D\u0938 \u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902 \u092C\u0924\u093E\
  \u092F\u093E \u0917\u092F\u093E \u0939\u0948."
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
