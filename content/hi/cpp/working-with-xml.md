---
title:                "XML के साथ काम करना"
date:                  2024-01-26T04:29:05.655149-07:00
model:                 gpt-4-0125-preview
simple_title:         "XML के साथ काम करना"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/working-with-xml.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
XML के साथ काम करना का मतलब है XML (एक्सटेंसिबल मार्कअप लैंग्वेज) डेटा का पार्सिंग, निर्माण, और मैनिपुलेशन करना। प्रोग्रामर इसकी प्लैटफ़ॉर्म-न्यूट्रल प्रकृति के कारण संरचित डेटा इंटरचेंज, कॉन्फ़िगरेशन, और अधिक को संभालने के लिए XML का प्रबंधन करते हैं।

## कैसे करें:
यहाँ TinyXML-2 लाइब्रेरी का उपयोग करके XML पार्स करने का एक सरल तरीका है:

```C++
#include <tinyxml2.h>
#include <iostream>

int main() {
    tinyxml2::XMLDocument doc;
    doc.Parse("<root><message>हैलो, वर्ल्ड!</message></root>");
    const char* content = doc.FirstChildElement("root")->FirstChildElement("message")->GetText();
    std::cout << content << std::endl;
    return 0;
}
```

नमूना आउटपुट:

```
हैलो, वर्ल्ड!
```

और यह है कि आप कैसे एक XML फाइल बनाते हैं:

```C++
#include <tinyxml2.h>
#include <iostream>

int main() {
    tinyxml2::XMLDocument doc;
    auto* declaration = doc.NewDeclaration();
    doc.InsertFirstChild(declaration);
    auto* root = doc.NewElement("root");
    doc.InsertEndChild(root);
    auto* message = doc.NewElement("message");
    message->SetText("हैलो, वर्ल्ड!");
    root->InsertEndChild(message);
    doc.SaveFile("output.xml");
    return 0;
}
```

इससे एक XML फाइल `output.xml` का निर्माण होता है जिसमें सामग्री होती है:

```xml
<?xml version="1.0"?>
<root>
    <message>हैलो, वर्ल्ड!</message>
</root>
```

## गहराई से समझना
XML 90 के दशक के अंत से वेब सेवाओं और डेटा स्टोरेज में केंद्रीय भूमिका निभाई है। जबकि अब JSON और YAML कॉन्फिग और इंटरोप के लिए अधिक सामान्य हैं, लेकिन XML अभी भी कई एंटरप्राइज़ सिस्टम्स में बहुत बड़ा है। C++ में XML पार्सिंग मैनुअल DOM/SAX पार्सिंग के साथ पुराने स्कूल की तरह महसूस कर सकती है। शुक्र है, टिनी XML-2 जैसी लाइब्रेरीज़ इसे सरलीकृत करती हैं। C++ का कोई बिल्ट-इन XML सपोर्ट नहीं है; टिनीXML-2, pugixml, या Xerces जैसी लाइब्रेरीज़ कठिन भागों को समेटती हैं।

## देखें भी
- टिनीXML-2 डॉक्यूमेंटेशन: https://leethomason.github.io/tinyxml2/
- pugixml लाइब्रेरी: https://pugixml.org/
- Xerces-C++ पार्सर: https://xerces.apache.org/xerces-c/
- W3C XML स्पेसिफिकेशन: https://www.w3.org/XML/
