---
title:                "HTML पार्स करना"
date:                  2024-01-20T15:30:49.183253-07:00
html_title:           "Bash: HTML पार्स करना"
simple_title:         "HTML पार्स करना"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

Parsing HTML एक ऐसी प्रक्रिया है जिसमें हम HTML डॉक्यूमेंट्स को पढ़ते हैं और उनके में छिपे डेटा और संरचना को समझते हैं। प्रोग्रामर्स इसे वेब पेजेस से जरूरी जानकारी प्राप्त करने या ऑटोमेशन के लिए करते हैं।

## How to: (कैसे करें:)

C++ में, आप HTML को पार्स करने के लिए किसी third-party library का इस्तेमाल करते हैं। Gumbo-parser एक ऐसी library है। यहां एक उदाहरण है:

```C++
#include <gumbo.h>
#include <iostream>

void search_for_links(GumboNode* node) {
    if (node->type != GUMBO_NODE_ELEMENT) {
        return;
    }
    
    GumboAttribute* href;
    if (node->v.element.tag == GUMBO_TAG_A &&
        (href = gumbo_get_attribute(&node->v.element.attributes, "href"))) {
        std::cout << href->value << std::endl;
    }

    for (unsigned int i = 0; i < node->v.element.children.length; ++i) {
        search_for_links(static_cast<GumboNode*>(node->v.element.children.data[i]));
    }
}

int main() {
    const char* html = "<html><body><a href='https://example.com'>Link</a></body></html>";
    GumboOutput* output = gumbo_parse(html);
    search_for_links(output->root);
    gumbo_destroy_output(&kGumboDefaultOptions, output);
}
```

नतीजा इस प्रकार होगा:

```plaintext
https://example.com
```

## Deep Dive (गहराई से जानकारी):

HTML पार्सिंग का इतिहास वेब की शुरुआत से चला आ रहा है। शुरुआत में, पार्सिंग बहुत ही बुनियादी थी और ज्यादातर रेगुलर एक्सप्रेशंस (Regular Expressions) पर निर्भर करती थी, जो न तो प्रभावी थी और न ही विश्वसनीय। आज, कई मजबूत libraries जैसे कि Gumbo-parser उपलब्ध हैं, जो HTML5 के स्पेसिफिकेशन का पालन करते हैं। 

अलग-अलग libraries में परफॉर्मेंस और API डिजाइन को लेकर विविधताएँ होती हैं। उदाहरण के लिए, BeautifulSoup और lxml जैसी libraries पायथन प्रोग्रामिंग में प्रयोग की जाती हैं। इसी तरह, कुछ प्रोग्रामर्स जावास्क्रिप्ट का उपयोग कर cheerio जैसे libraries को पसंद करते हैं। 

जैसे कि Gumbo-parser का उदाहरण दिया गया है, C++ में HTML पार्सिंग करते समय आप डोम (DOM) ट्री को ट्रॅवर्स कर सकते हैं और नोड्स पर विचार कर सकते हैं। हालांकि, C++ की स्टैंडर्ड लाइब्रेरी में HTML पार्सिंग के लिए कोई नेटिव सपोर्ट नहीं है, इसीलिए हम third-party libraries का सहारा लेते हैं।

## See Also (और जानकारी के लिए):

- Gumbo-parser GitHub: https://github.com/google/gumbo-parser
- HTML5 Parsing algorithm: https://html.spec.whatwg.org/multipage/parsing.html
- W3C's list of HTML parsing libraries: https://www.w3.org/2002/02/mid/4D5AAB6A.2011%40prescod.net