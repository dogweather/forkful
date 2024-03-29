---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 18:15:10.149758-07:00
description: "C \u092E\u0947\u0902 XML \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\
  \u092E \u0915\u0930\u0928\u093E \u0935\u093F\u092D\u093F\u0928\u094D\u0928 \u0932\
  \u093E\u0907\u092C\u094D\u0930\u0947\u0930\u093F\u092F\u094B\u0902 \u0915\u093E\
  \ \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\u0947 XML \u0926\u0938\u094D\
  \u0924\u093E\u0935\u0947\u091C\u094B\u0902 \u0915\u094B \u092A\u093E\u0930\u094D\
  \u0938 \u0915\u0930\u0928\u093E, \u092A\u0942\u091B\u0924\u093E\u091B \u0915\u0930\
  \u0928\u093E, \u0914\u0930 \u0939\u0947\u0930\u092B\u0947\u0930 \u0915\u0930\u0928\
  \u093E \u0936\u093E\u092E\u093F\u0932 \u0939\u0948\u0964 \u092A\u094D\u0930\u094B\
  \u0917\u094D\u0930\u093E\u092E\u0930 \u0935\u0947\u092C \u0938\u0947\u0935\u093E\
  \u0913\u0902,\u2026"
lastmod: '2024-03-13T22:44:53.189152-06:00'
model: gpt-4-0125-preview
summary: "C \u092E\u0947\u0902 XML \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E\
  \ \u0915\u0930\u0928\u093E \u0935\u093F\u092D\u093F\u0928\u094D\u0928 \u0932\u093E\
  \u0907\u092C\u094D\u0930\u0947\u0930\u093F\u092F\u094B\u0902 \u0915\u093E \u0909\
  \u092A\u092F\u094B\u0917 \u0915\u0930\u0915\u0947 XML \u0926\u0938\u094D\u0924\u093E\
  \u0935\u0947\u091C\u094B\u0902 \u0915\u094B \u092A\u093E\u0930\u094D\u0938 \u0915\
  \u0930\u0928\u093E, \u092A\u0942\u091B\u0924\u093E\u091B \u0915\u0930\u0928\u093E\
  , \u0914\u0930 \u0939\u0947\u0930\u092B\u0947\u0930 \u0915\u0930\u0928\u093E \u0936\
  \u093E\u092E\u093F\u0932 \u0939\u0948\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\
  \u0930\u093E\u092E\u0930 \u0935\u0947\u092C \u0938\u0947\u0935\u093E\u0913\u0902\
  ,\u2026"
title: "XML \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\u093E"
---

{{< edit_this_page >}}

## क्या और क्यों?

C में XML के साथ काम करना विभिन्न लाइब्रेरियों का उपयोग करके XML दस्तावेजों को पार्स करना, पूछताछ करना, और हेरफेर करना शामिल है। प्रोग्रामर वेब सेवाओं, कॉन्फ़िगरेशन फ़ाइलों, और विभिन्न प्रणालियों के बीच डेटा आदान-प्रदान में इसके व्यापक उपयोग के कारण XML के साथ जुड़ते हैं, जो मजबूत एप्लिकेशन विकास के लिए XML को कुशलतापूर्वक संभालने के कौशल की आवश्यकता को जन्म देता है।

## कैसे करें:

C में XML के लिए बिल्ट-इन समर्थन नहीं है, इसलिए आपको बाहरी लाइब्रेरियों का उपयोग करना होगा। एक लोकप्रिय विकल्प `libxml2` है, जो एक स्थिर और फीचर-समृद्ध लाइब्रेरी है। यहाँ `libxml2` का उपयोग करके एक XML फ़ाइल को पढ़ने और पार्स करने का तरीका दिया गया है।

सबसे पहले, सुनिश्चित करें कि आपके सिस्टम पर `libxml2` स्थापित है। आपको इसे अपने पैकेज मैनेजर के माध्यम से स्थापित करना पड़ सकता है (उदाहरण के तौर पर, डेबियन सिस्टम्स पर `apt-get install libxml2-dev`)

अगला, अपने C प्रोग्राम में `libxml2` हेडर शामिल करें:
```c
#include <libxml/parser.h>
#include <libxml/tree.h>
```

अब, चलिए एक साधारण प्रोग्राम लिखते हैं जो एक XML फाइल को पार्स करता है और पहले स्तर के तत्वों के नामों को प्रिंट करता है:
```c
#include <stdio.h>
#include <libxml/parser.h>
#include <libxml/tree.h>

int main(void) {
    xmlDoc *document = NULL;
    xmlNode *root_element = NULL;

    // लाइब्रेरी को इनिशियलाइज करें और संभावित ABI असंगतियों की जांच करें
    LIBXML_TEST_VERSION

    // फाइल को पार्स करें और DOM प्राप्त करें
    document = xmlReadFile("your_file.xml", NULL, 0);

    if (document == NULL) {
        printf("XML फ़ाइल को पार्स करने में विफल।\n");
        return -1;
    }

    // मूल तत्व नोड प्राप्त करें
    root_element = xmlDocGetRootElement(document);

    for (xmlNode *currentNode = root_element; currentNode; currentNode = currentNode->next) {
        if (currentNode->type == XML_ELEMENT_NODE) {
            printf("नोड प्रकार: तत्व, नाम: %s\n", currentNode->name);
        }
    }

    // पार्सर और DOM के लिए आवंटित स्मृति को मुक्त करें
    xmlFreeDoc(document);

    // सफाई और लीक की जांच
    xmlCleanupParser();
    xmlMemoryDump(); // वैकल्पिक

    return 0;
}
```

इस प्रोग्राम को कंपाइल करने के लिए, `libxml2` के खिलाफ लिंक करना सुनिश्चित करें:
```sh
gcc -o xml_example xml_example.c $(xml2-config --cflags --libs)
```

यदि आपके पास `your_file.xml` नामक एक XML फाइल है, तो कंपाइल किया गया प्रोग्राम चलाने पर इसके पहले स्तर के तत्वों के नाम प्रिंट करना चाहिए।

## गहराई से विचार

C और XML के बीच की बातचीत दो विशाल रूप से अलग दुनियाओं को एक साथ लाने की कहानी है: C की संरचित, बाइट-स्तरीय, प्रक्रियात्मक पैराडाइम और XML का पदानुक्रमिक, वर्बोज, और दस्तावेज़-केंद्रित मॉडल। जब C प्रोग्रामों में XML हैंडलिंग की क्षमताओं को एकीकृत करते हैं, तो डेवलपर्स C की ताकतों - जैसे कि गति और कम-स्तरीय मेमोरी पहुंच - का लाभ उठाते हैं ताकि वे XML दस्तावेजों को कुशलतापूर्वक पार्स और हेरफेर कर सकें।

`libxml2`, GNOME प्रोजेक्ट के हिस्से के रूप में विकसित, C में XML प्रोसेसिंग के लिए de facto मानक के रूप में उभरा क्योंकि इसमें XML मानकों के लिए व्यापक समर्थन और इसकी प्रदर्शन क्षमता थी। इसमें वर्षों के विकास प्रयास और समुदाय योगदान शामिल हैं, जो इसे अधिकांश XML कार्यों के लिए मजबूत और कुशल बनाता है।

हालांकि `libxml2` शक्तिशाली क्षमताएँ प्रदान करता है, यह उल्लेखनीय है कि XML पार्सिंग और हेरफेर की जटिलता महत्वपूर्ण ओवरहेड पेश कर सकती है। जहां XML की वाचालता और जटिलता अनुचित हो, वहाँ डेटा आदान-प्रदान के लिए JSON जैसे विकल्प प्राथमिकता हो सकते हैं। फिर भी, XML-केंद्रित अनुप्रयोगों या ऐसे वातावरणों के लिए जहां XML का उपयोग प्रचलित है, C में `libxml2` के उपयोग को महारत हासिल करना व्यापक रेंज के XML दस्तावेजों और APIs के साथ काम करने की क्षमता प्रदान करता है, C प्रोग्रामिंग भाषा और संरचित दस्तावेज प्रोसेसिंग की दुनिया के बीच एक पुल का निर्माण करता है।
