---
title:                "XML के साथ काम करना"
aliases:
- /hi/arduino/working-with-xml/
date:                  2024-01-26T04:28:30.004787-07:00
model:                 gpt-4-0125-preview
simple_title:         "XML के साथ काम करना"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/working-with-xml.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
अरडुइनो पर XML के साथ काम करना वेब APIs या कॉन्फ़िगरेशन फाइलों से आने वाले XML डेटा को पार्स करने और मैनिपुलेट करने का काम शामिल है। प्रोग्रामर्स इसे उन सेवाओं के साथ इंटीग्रेट करने के लिए करते हैं जो डेटा एक्सचेंज के लिए XML का उपयोग करते हैं या डेटा को संरचित, मानव-पठनीय फॉर्मेट में स्टोर करने के लिए।

## कैसे:
हम XML बनाने के लिए `XMLWriter` लाइब्रेरी का और इसे पार्स करने के लिए `tinyxml2` लाइब्रेरी का उपयोग करेंगे। पहले अपने अरडुइनो IDE में लाइब्रेरी मैनेजर के माध्यम से लाइब्रेरी को इंस्टॉल करें।

XML दस्तावेज़ बनाना:

```Arduino
#include <XMLWriter.h>

void setup() {
  Serial.begin(9600);
  
  XMLWriter xml(&Serial); // Serial का उपयोग करके आउटपुट
  
  xml.header();
  xml.tag("greeting").tag("text").text("नमस्ते, दुनिया!").close().close();
  xml.flush();
}

void loop() {
}
```

एक XML स्ट्रिंग को डिकोड करना:

```Arduino
#include <tinyxml2.h>

tinyxml2::XMLDocument doc;
doc.Parse("<greeting><text>नमस्ते, दुनिया!</text></greeting>");

tinyxml2::XMLElement* text = doc.FirstChildElement("greeting")->FirstChildElement("text");
if (text != nullptr) {
  Serial.println(text->GetText());
}
```

नमूना आउटपुट:

```
<greeting>
  <text>नमस्ते, दुनिया!</text>
</greeting>
```

## गहराई से जानना
XML, या Extensible Markup Language, एक मार्कअप भाषा है जो दस्तावेज़ों को एक मानव-पठनीय और मशीन-पठनीय फॉर्मेट में कोडिंग के लिए एक सेट नियमों को परिभाषित करती है। यह 90 के दशक के अंत से मौजूद है और विभिन्न क्षेत्रों में व्यापक रूप से उपयोग की जाती है, विशेष रूप से जहां प्लेटफॉर्म-इंडिपेंडेंट डेटा एक्सचेंज की आवश्यकता होती है। अरडुइनो के सीमित मेमोरी संसाधन XML के साथ काम करना PC पर की तुलना में अधिक चुनौतीपूर्ण बनाते हैं। इसलिए, हल्की लाइब्रेरी महत्वपूर्ण होती हैं। हालांकि JSON डेटा एक्सचेंज के लिए इसके सरल सिंटेक्स और छोटे फुटप्रिंट के कारण लोकप्रियता प्राप्त कर रहा है, XML अभी भी व्यापक रूप से उपयोग किया जाता है, विशेष रूप से जब पुराने सिस्टमों या स्कीमाओं के माध्यम से दस्तावेज़ सत्यापन की आवश्यकता होने वाले अनुप्रयोगों के साथ निपटने में। अरडुइनो XML कार्यान्वयन की कुंजी स्ट्रीम पार्सिंग है, जो दस्तावेज़ को खंडों में पढ़ता है ताकि मेमोरी उपयोग कम रखा जा सके।

## और देखें
- [TinyXML-2 लाइब्रेरी डॉक्यूमेंटेशन](https://leethomason.github.io/tinyxml2/)
- JSON डेटा के साथ काम करते समय एक विकल्प के रूप में [अरडुइनो JSON लाइब्रेरी](https://arduinojson.org/)।
- सामान्य XML सीखने के लिए [W3Schools XML ट्यूटोरियल](https://www.w3schools.com/xml/)।
- आधिकारिक XML मानकों और सिफारिशों के लिए [W3C XML स्पेसिफिकेशन](https://www.w3.org/XML/)।
