---
title:                "XML के साथ काम करना"
date:                  2024-01-26T04:36:27.090132-07:00
model:                 gpt-4-0125-preview
simple_title:         "XML के साथ काम करना"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/working-with-xml.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
XML के साथ काम करने का मतलब है, कोड का उपयोग करके XML (एक्सटेंसिबल मार्कअप लैंग्वेज) दस्तावेज़ों को पार्स करना, जेनरेट करना, और संशोधित करना। प्रोग्रामर इसे कई वेब सेवाओं, कॉन्फ़िग फाइलों, और डेटा इंटरचेंज प्रारूपों के साथ संवाद करने के लिए करते हैं जहां XML लिंग्वा फ्रेंका है।

## कैसे:
चलिए XML स्निपट को पार्स करने के लिए Ruby के साथ शामिल REXML का उपयोग करते हैं:
```Ruby
require 'rexml/document'
include REXML

xml_data = <<-XML
<fruits>
  <fruit name="apple" color="green"/>
  <fruit name="banana" color="yellow"/>
</fruits>
XML

document = Document.new(xml_data)
document.elements.each('fruits/fruit') do |element|
  puts "Name: #{element.attributes['name']}, Color: #{element.attributes['color']}"
end
```
आउटपुट:
```
नाम: apple, रंग: हरा
नाम: banana, रंग: पीला
```

XML को जेनरेट करना भी सीधा है:
```Ruby
doc = Document.new
doc.add_element 'fruits'
apple = doc.root.add_element 'fruit', {'name' => 'apple', 'color' => 'green'}
banana = doc.root.add_element 'fruit', {'name' => 'banana', 'color' => 'yellow'}
puts doc
```
XML आउटपुट:
```XML
<fruits>
  <fruit name="apple" color="green"/>
  <fruit name="banana" color="yellow"/>
</fruits>
```

## गहराई से समझना:
XML की जड़ें 1990 के दशक में हैं जब यह वेब दस्तावेज़ों के लिए SGML का एक सरलीकृत उपसमूह था। यह बहुत बड़ा है लेकिन अत्यधिक संरचित है, और यही कारण है कि यह चलन में है। यह यहाँ पर एकमात्र विकल्प नहीं है—JSON और YAML उनकी सादगी के कारण लोकप्रिय हो गए हैं—लेकिन XML कई एंटरप्राइज और विरासती सिस्टमों में मजबूती से टिका हुआ है।

Ruby XML से निपटने के लिए कुछ तरीके प्रदान करता है। REXML एक सरल Ruby लाइब्रेरी है जिसे शुरू करना आसान है। Nokogiri एक रत्न है जो तेज C लाइब्रेरियों को लपेटता है, गति और अतिरिक्त सुविधाएँ प्रदान करता है। बीच में चयन करना? छोटे कार्यों के लिए REXML के साथ शुरू करें और अधिक शक्ति की आवश्यकता हो तो Nokogiri पर जाएं।

अंदरुनी तौर पर, XML को पार्स करना स्ट्रिंग्स को DOM या SAX मॉडल में अनुवादित करने के बारे में है। DOM एक वृक्ष को मेमोरी में बनाता है, जबकि SAX दस्तावेज़ को स्ट्रीम करता है और जैसे ही यह पार्स करता है, ईवेंट को ट्रिगर करता है। REXML दोनों मॉडल प्रदान करता है, लेकिन Nokogiri जैसे C एक्सटेंशन की तुलना में धीमा होता है।

## देखें भी:
- Ruby REXML दस्तावेज़ीकरण: https://www.rubydoc.info/stdlib/rexml
- Nokogiri रत्न: https://nokogiri.org/
- XML विनिर्देश: https://www.w3.org/XML/
- SAX का परिचय: https://www.saxproject.org/
- YAML बनाम JSON बनाम XML तुलना: https://www.upwork.com/resources/json-vs-xml-vs-yaml