---
title:                "XML के साथ काम करना"
date:                  2024-01-26T04:28:58.445941-07:00
model:                 gpt-4-0125-preview
simple_title:         "XML के साथ काम करना"

category:             "C"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/c/working-with-xml.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
C में XML के साथ काम करना XML फ़ाइलों को पार्स करना, बनाना, और मैनिपुलेट करना शामिल है - मूल रूप से संरचित डाटा स्टोरेज। प्रोग्रामर इसे पोर्टेबल और इंसानों द्वारा पढ़े जाने योग्य प्रारूप में डाटा के साथ संवाद करने के लिए करते हैं, अक्सर इसका उपयोग कॉन्फ़िगरेशन, डाटा एक्सचेंज, और अधिक के लिए किया जाता है।

## कैसे करें:
नीचे एक छोटा स्निपेट है जो `libxml2` लाइब्रेरी का उपयोग करके एक XML फ़ाइल को पार्स करने और रूट एलिमेंट प्राप्त करने के लिए है।

```C
#include <stdio.h>
#include <libxml/parser.h>
#include <libxml/tree.h>

int main() {
    xmlDoc *doc = NULL;
    xmlNode *root_element = NULL;

    // XML फ़ाइल को पार्स करें
    doc = xmlReadFile("example.xml", NULL, 0);

    // रूट एलिमेंट प्राप्त करें
    root_element = xmlDocGetRootElement(doc);

    printf("Root Element: %s\n", root_element->name);

    // दस्तावेज़ को मुक्त करें
    xmlFreeDoc(doc);

    // पार्सर की सफाई
    xmlCleanupParser();

    return 0;
}
```

जड़ `<data>` वाले XML के लिए नमूना आउटपुट हो सकता है:
```
Root Element: data
```

## गहन विश्लेषण
XML, या Extensible Markup Language, 90 के दशक के अंत से डाटा को वर्णन और संरचना करने का एक तरीका प्रदान करता है। C में, `libxml2` पहला विकल्प है। यह रोबस्ट है, हालांकि XML नौसिखियों के लिए सबसे आसान नहीं है। विकल्पों में `tinyxml2` शामिल है, जो हल्का और अधिक शुरुआत-अनुकूल है। क्रियान्वयन के लिए, C में बिल्ट-इन XML समर्थन नहीं है, इसलिए लाइब्रेरीज इस अंतर को भरती हैं। वे आकार, गति, जटिलता, और पोर्टबिलिटी में भिन्न होती हैं। अधिकांश DOM और SAX पार्सिंग विधियों की पेशकश करते हैं: DOM पूरी चीज़ को मेमोरी में लोड करता है, छोटे दस्तावेजों के लिए अच्छा; SAX इवेंट-संचालित है, तत्वों को तुरंत संभालता है, बड़ी फाइलों के लिए बेहतर। दोनों के अपने-अपने उपयोग के मामले और ट्रेड-ऑफ़ हैं।

## देखें भी
- [libxml2](http://xmlsoft.org/)
- [GitHub पर tinyxml2](https://github.com/leethomason/tinyxml2)
- [w3schools पर XML ट्यूटोरियल](https://www.w3schools.com/xml/)
- [W3C द्वारा XML विशिष्टता](https://www.w3.org/XML/)
