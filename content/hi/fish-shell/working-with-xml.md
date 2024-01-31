---
title:                "XML के साथ काम करना"
date:                  2024-01-26T04:31:44.509814-07:00
model:                 gpt-4-0125-preview
simple_title:         "XML के साथ काम करना"

category:             "Fish Shell"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/working-with-xml.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
XML के साथ काम करना का मतलब है डेटा को एक व्यापक, संरचित प्रारूप में व्यवस्थित करना, जिसका उपयोग कॉन्फ़िगरेशन, मैसेजिंग और अधिक में किया जाता है। प्रोग्रामर XML को पढ़ने, लिखने, अपडेट करने, और डेटा को क्वेरी करने के लिए संशोधित करते हैं—अनेक ऐप्स और सेवाओं में अंतर्संचालकता के लिए अत्यावश्यक।

## कैसे करें:
Fish में बिल्ट-इन XML पार्सिंग नहीं है, इसलिए आप `xmllint` या `xmlstarlet` जैसे बाहरी उपकरणों पर निर्भर रहेंगे। यहाँ मानों को पढ़ने के लिए एक स्निपेट है:

```fish
# xmlstarlet का उपयोग करके XML पार्स करें
echo '<root><element>Hello World</element></root>' | xmlstarlet sel -t -v "/root/element"
```

आउटपुट:
```
Hello World
```

XML में संपादन के लिए, इसका उपयोग करें:

```fish
# xmlstarlet का उपयोग करके XML तत्व में संपादन करें
echo '<root><element>Old Value</element></root>' | xmlstarlet ed -u "/root/element" -v 'New Value'
```

आउटपुट:
```xml
<?xml version="1.0"?>
<root>
  <element>New Value</element>
</root>
```

## गहराई में:
XML '90 के दशक के अंत से आसपास है, पठनीयता और मशीन-मित्रता के लिए बनाया गया। जबकि JSON की सिम्प्लिसिटी के कारण XML की लोकप्रियता कुछ हद तक कम हुई है, डॉक्यूमेंट वैलिडेशन और नामस्थानों में महत्वपूर्ण होने के कारण XML अब भी मजबूती से स्थापित है।

विकल्प? निश्चित—JSON, YAML, या यहाँ तक कि प्रदर्शन-गहन ऐप्स के लिए बाइनरी प्रारूप जैसे कि प्रोटोकॉल बफर्स। लेकिन XML का स्कीमा और XSLT (XML परिवर्तनों के लिए) जटिल परिदृश्यों में महत्वपूर्ण हो सकते हैं जहां मजबूतता मायने रखती है।

अंतर्निहित रूप से, `xmlstarlet` जैसे उपकरण libxml2 जैसी शक्तिशाली लाइब्रेरी को लपेटते हैं, आपको XPath और XQuery जैसे सूक्ष्म-ग्रेनुलेटी वाली XML टिंकरिंग के लिए सौंपते हैं। ये केवल XML उपकरण नहीं हैं बल्कि DOM मैनिपुलेशन के लिए द्वार हैं, जैसा कि आप XML से छूने वाली किसी भी भाषा में समान अवधारणाओं को लागू करेंगे।

## देखें भी:
- [xmlstarlet दस्तावेज़ीकरण](http://xmlstar.sourceforge.net/doc/UG/xmlstarlet-ug.html)
- [Fish दस्तावेज़ीकरण](https://fishshell.com/docs/current/index.html)
- [XPath और XQuery फ़ंक्शन्स और ऑपरेटर्स](https://www.w3.org/TR/xpath-functions/)
