---
title:                "HTML पार्स करना"
html_title:           "C++: HTML पार्स करना"
simple_title:         "HTML पार्स करना"
programming_language: "PHP"
category:             "PHP"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/php/parsing-html.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

HTML पार्सिंग का मतलब है एक HTML कोड को विश्लेषित करना और उसकी संरचना को समझना। प्रोग्रामर इसे एक HTML डॉक्यूमेंट की संरचना और उसमें मौजूद डाटा को पढ़ने और मोडिफ़ाई करने के लिए करते हैं।

## कैसे:

आओ HTML पार्सिंग के लिए कुछ साधारण PHP कोड देखें:

```PHP
<?php
$doc = new DOMDocument();
libxml_use_internal_errors(TRUE);  // turn off error for handling html
$doc->loadHTML('<html><body><p>Hello World!</p></body></html>');
libxml_clear_errors();
$xp = new DOMXPath($doc);
$p = $xp->query('//p')->item(0);
echo $p->textContent;
?>
```

यह कोड एक HTML डॉक्यूमेंट लोड करता है और 'p' टैग के साथ पहली आइटम को निकालता है। फिर, यह उसकी टेक्स्ट सामग्री को प्रिंट करता है। आपको "Hello World!" के रूप में उत्तर मिलेगा।

## गहरा डुबकी:

1. **ऐतिहासिक संदर्भ:** HTML पार्सिंग का उपयोग वेब स्क्रेपिंग के लिए सामान्य तौर पर किया जाता है, जब सैकड़ों पेज से डेटा एकत्र करने की जरूरत हो।
2. **विकल्प:** `DOMDocument` के अतिरिक्त, आप `SimpleHTMLDOM` और `QueryPath` जैसे अन्य HTML पार्सिंग लाइब्रेरीज़ भी उपयोग कर सकते हैं।
3. **कार्यान्वयन विवरण:** `DOMDocument` और `DOMXPath` PHP वर्गों का उपयोग करके हम HTML डॉक्यूमेंट के विभिन्न हिस्सों को पहुंच सकते हैं। `loadHTML()` से हम HTML डॉक्यूमेंट को लोड कर सकते हैं और `query()` से हम निश्चित टैग के साथ आइटमों को निकाल सकते हैं।

## और देखें:

1. PHP दस्तावेज़ - DOM: https://www.php.net/manual/en/book.dom.php
2. PHP दस्तावेज़ - XPath: https://www.php.net/manual/en/class.domxpath.php
3. Simple HTML DOM Parser: http://simplehtmldom.sourceforge.net/
4. QueryPath: https://querypath.org/