---
title:                "HTML विश्लेषण"
aliases:
- /hi/cpp/parsing-html/
date:                  2024-02-03T19:12:16.222302-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTML विश्लेषण"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
HTML का पार्सिंग का अर्थ है HTML सामग्री को किसी ऐसे रूप में तोड़ना जो कि एक कार्यक्रम समझ और संशोधित कर सके। प्रोग्रामर इसे डेटा निकालने, सामग्री को संशोधित करने या अपने अनुप्रयोगों में वेब स्क्रेपिंग को एकीकृत करने के लिए करते हैं।

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
