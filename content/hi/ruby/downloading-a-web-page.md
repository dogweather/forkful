---
title:                "एक वेब पेज डाउनलोड करना"
html_title:           "Kotlin: एक वेब पेज डाउनलोड करना"
simple_title:         "एक वेब पेज डाउनलोड करना"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## क्या और क्यों? 
एक वेब पेज डाउनलोड करना मतलब उसकी सामग्री को अपने कंप्यूटर पर सांचित करना। प्रोग्रामर इसे इसलिए करते हैं ताकि वे वेबसाइट की डेटा का विश्लेषण और प्रसंस्करण कर सकें। 

## कैसे करें: 
कोर Ruby में, आप `Net::HTTP` लाइब्रेरी का उपयोग करके वेब पेज डाउनलोड कर सकते हैं। 

```Ruby
require 'net/http'

http = Net::HTTP.new('www.example.com', 80)
response = http.get('/')

puts response.body
```

यह कोड उदाहरण 'www.example.com' नामक साइट से वेब पेज प्राप्त करेगा।

## गहराई में: 
वेब पेज की डाउनलोडिंग 1990 के दशक के मध्य से ही हुई थी, जब World Wide Web का आविष्कार हुआ था। वैकल्पिक रूप से, आप `open-uri` और `nokogiri` जैसी गेम्स का उपयोग कर सकते हैं, जो वेब पेजेज को प्राप्त करने और पार्स करने में अधिक कुशल हैं। `Net::HTTP` से कम्‍प्‍लेक्‍स उपयोग के अवसरों के लिए, 302 पुनर्निर्देशन, कूकीज आदि के साथ संभालता है। 

## इसके अलावा देखें: 
1. [Open URI documentation](https://ruby-doc.org/stdlib-2.5.1/libdoc/open-uri/rdoc/OpenURI.html)
2. [Nokogiri Tutorial: Parsing HTML](https://nokogiri.org/tutorials/parsing_an_html_xml_document.html)
3. [Net::HTTP documentation](https://ruby-doc.org/stdlib-2.7.0/libdoc/net/http/rdoc/Net/HTTP.html)