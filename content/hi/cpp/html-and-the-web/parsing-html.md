---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:16.222302-07:00
description: "\u0915\u0948\u0938\u0947: C++ \u092E\u0947\u0902 \u092C\u093F\u0932\u094D\
  \u091F-\u0907\u0928 HTML \u092A\u093E\u0930\u094D\u0938\u093F\u0902\u0917 \u0915\
  \u094D\u0937\u092E\u0924\u093E\u090F\u0902 \u0928\u0939\u0940\u0902 \u0906\u0924\
  \u0940\u0902\u0964 \u0906\u092A \u0905\u0915\u094D\u0938\u0930 \u0917\u0942\u0917\
  \u0932 \u0915\u093E \u0917\u0902\u092C\u094B-\u092A\u093E\u0930\u094D\u0938\u0930\
  \ \u092F\u093E \u0915\u0941\u091B \u0907\u0938\u0940 \u0924\u0930\u0939 \u0915\u0940\
  \ \u0932\u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940 \u0915\u093E \u0909\u092A\
  \u092F\u094B\u0917 \u0915\u0930\u0947\u0902\u0917\u0947\u0964 \u092F\u0939\u093E\
  \u0901 \u0917\u0902\u092C\u094B-\u092A\u093E\u0930\u094D\u0938\u0930 \u0915\u093E\
  \u2026"
lastmod: '2024-03-13T22:44:52.840783-06:00'
model: gpt-4-0125-preview
summary: "C++ \u092E\u0947\u0902 \u092C\u093F\u0932\u094D\u091F-\u0907\u0928 HTML\
  \ \u092A\u093E\u0930\u094D\u0938\u093F\u0902\u0917 \u0915\u094D\u0937\u092E\u0924\
  \u093E\u090F\u0902 \u0928\u0939\u0940\u0902 \u0906\u0924\u0940\u0902\u0964 \u0906\
  \u092A \u0905\u0915\u094D\u0938\u0930 \u0917\u0942\u0917\u0932 \u0915\u093E \u0917\
  \u0902\u092C\u094B-\u092A\u093E\u0930\u094D\u0938\u0930 \u092F\u093E \u0915\u0941\
  \u091B \u0907\u0938\u0940 \u0924\u0930\u0939 \u0915\u0940 \u0932\u093E\u0907\u092C\
  \u094D\u0930\u0947\u0930\u0940 \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\
  \u0930\u0947\u0902\u0917\u0947\u0964 \u092F\u0939\u093E\u0901 \u0917\u0902\u092C\
  \u094B-\u092A\u093E\u0930\u094D\u0938\u0930 \u0915\u093E \u0909\u092A\u092F\u094B\
  \u0917 \u0915\u0930\u0924\u0947 \u0939\u0941\u090F \u090F\u0915 \u0924\u094D\u0935\
  \u0930\u093F\u0924 \u0909\u0926\u093E\u0939\u0930\u0923 \u0939\u0948."
title: "HTML \u0935\u093F\u0936\u094D\u0932\u0947\u0937\u0923"
weight: 43
---

## कैसे:
C++ में बिल्ट-इन HTML पार्सिंग क्षमताएं नहीं आतीं। आप अक्सर गूगल का गंबो-पार्सर या कुछ इसी तरह की लाइब्रेरी का उपयोग करेंगे। यहाँ गंबो-पार्सर का उपयोग करते हुए एक त्वरित उदाहरण है:

```C++
#include <iostream>
#include <gumbo.h>

void search_for_links(GumboNode* node) {
    if (node->type != GUMBO_NODE_ELEMENT) {
        return;
    }
    if (node->v.element.tag == GUMBO_TAG_A) {
        GumboAttribute* href = gumbo_get_attribute(&node->v.element.attributes, "href");
        if (href) {
            std::cout << href->value << std::endl;
        }
    }
    GumboVector* children = &node->v.element.children;
    for (unsigned int i = 0; i < children->length; ++i) {
        search_for_links(static_cast<GumboNode*>(children->data[i]));
    }
}

int main() {
    const char* html = "<html><body><a href='https://example.com'>Link</a></body></html>";
    GumboOutput* output = gumbo_parse(html);
    search_for_links(output->root);
    gumbo_destroy_output(&kGumboDefaultOptions, output);
    return 0;
}
```

नमूना आउटपुट:
```
https://example.com
```

## गहराई से जानकारी
C++ में HTML का पार्सिंग हमेशा से सरल नहीं रहा है। ऐतिहासिक रूप से, प्रोग्रामर्स रेगेक्स या हस्त लिखित पार्सर्स का उपयोग करते थे, जो दोनों ही त्रुटि प्रवण और जटिल होते हैं। आजकल, गंबो-पार्सर जैसी मजबूत लाइब्रेरियां पार्सिंग की जटिलताओं को संभालती हैं, जिससे इसे आसान और अधिक विश्वसनीय बना दिया गया है।

विकल्पों में टिडी, मायएचटीएमएल, या यहां तक कि C++ `system` फ़ंक्शन या एम्बेडेड इंटरप्रिटर्स के माध्यम से पायथन के सुपरफेल के साथ C++ का एकीकरण शामिल है।

कार्यान्विति के हिसाब से, ये लाइब्रेरियां HTML को एक डॉक्यूमेंट ऑब्जेक्ट मॉडल (DOM) ट्री में परिवर्तित करती हैं। DOM को ट्रैवर्स करना और मैनिपुलेट करना उपयोगकर्ताओं को डेटा निकालने और उसके साथ काम करने की अनुमति देता है जैसा कि कैसे खंड में दर्शाया गया है।

## देखें भी
- [गंबो-पार्सर GitHub रेपोजिटरी](https://github.com/google/gumbo-parser)
- [HTML पार्सिंग लाइब्रेरियों की सूची](https://en.cppreference.com/w/c/experimental/dynamic)
- [C++ और पायथन इंटरऑपरेबिलिटी](https://docs.python.org/3/extending/embedding.html)
