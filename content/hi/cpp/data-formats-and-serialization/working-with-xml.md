---
aliases:
- /hi/cpp/working-with-xml/
date: 2024-01-26 04:29:05.655149-07:00
description: "XML \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\
  \u093E \u0915\u093E \u092E\u0924\u0932\u092C \u0939\u0948 XML (\u090F\u0915\u094D\
  \u0938\u091F\u0947\u0902\u0938\u093F\u092C\u0932 \u092E\u093E\u0930\u094D\u0915\u0905\
  \u092A \u0932\u0948\u0902\u0917\u094D\u0935\u0947\u091C) \u0921\u0947\u091F\u093E\
  \ \u0915\u093E \u092A\u093E\u0930\u094D\u0938\u093F\u0902\u0917, \u0928\u093F\u0930\
  \u094D\u092E\u093E\u0923, \u0914\u0930 \u092E\u0948\u0928\u093F\u092A\u0941\u0932\
  \u0947\u0936\u0928 \u0915\u0930\u0928\u093E\u0964 \u092A\u094D\u0930\u094B\u0917\
  \u094D\u0930\u093E\u092E\u0930 \u0907\u0938\u0915\u0940 \u092A\u094D\u0932\u0948\
  \u091F\u092B\u093C\u0949\u0930\u094D\u092E-\u0928\u094D\u092F\u0942\u091F\u094D\u0930\
  \u0932\u2026"
lastmod: 2024-02-18 23:09:03.934539
model: gpt-4-0125-preview
summary: "XML \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\
  \u093E \u0915\u093E \u092E\u0924\u0932\u092C \u0939\u0948 XML (\u090F\u0915\u094D\
  \u0938\u091F\u0947\u0902\u0938\u093F\u092C\u0932 \u092E\u093E\u0930\u094D\u0915\u0905\
  \u092A \u0932\u0948\u0902\u0917\u094D\u0935\u0947\u091C) \u0921\u0947\u091F\u093E\
  \ \u0915\u093E \u092A\u093E\u0930\u094D\u0938\u093F\u0902\u0917, \u0928\u093F\u0930\
  \u094D\u092E\u093E\u0923, \u0914\u0930 \u092E\u0948\u0928\u093F\u092A\u0941\u0932\
  \u0947\u0936\u0928 \u0915\u0930\u0928\u093E\u0964 \u092A\u094D\u0930\u094B\u0917\
  \u094D\u0930\u093E\u092E\u0930 \u0907\u0938\u0915\u0940 \u092A\u094D\u0932\u0948\
  \u091F\u092B\u093C\u0949\u0930\u094D\u092E-\u0928\u094D\u092F\u0942\u091F\u094D\u0930\
  \u0932\u2026"
title: "XML \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\u093E"
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
