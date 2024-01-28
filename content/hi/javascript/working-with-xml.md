---
title:                "XML के साथ काम करना"
date:                  2024-01-26T04:33:44.729635-07:00
model:                 gpt-4-0125-preview
simple_title:         "XML के साथ काम करना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/working-with-xml.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
XML के साथ काम करना का मतलब है कोड का उपयोग करके XML सामग्री का पार्स करना, हेरफेर करना और उत्पादन करना। प्रोग्रामर इसे करते हैं क्योंकि XML, इसके मानव-पठनीय और मशीन-पार्सयोग्य स्वभाव के कारण, कॉन्फ़िगरेशन फ़ाइल, डेटा एक्सचेंज, और वेब सेवाओं के लिए व्यापक रूप से उपयोग किया जाता है।

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
