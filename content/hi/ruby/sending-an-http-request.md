---
title:                "एक एचटिट्पी अनुरोध भेजना"
html_title:           "Ruby: एक एचटिट्पी अनुरोध भेजना"
simple_title:         "एक एचटिट्पी अनुरोध भेजना"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

## यह क्या है और क्यों?
HTTP अनुरोध भेजना क्या होता है और इसे क्यों प्रोग्रामर्स करते हैं, इसको समझना महत्वपूर्ण है। HTTP अनुरोध भेजना साधारणतया एक वेब ऐप्लिकेशन के साथ कुछ डेटा भेजने का तरीका है, परिणामस्वरूप उस वेब साइट से आपको उपयुक्त डेटा मिलता है।

## कैसे करें:
```Ruby
require 'net/http'
uri = URI('https://www.example.com/')
response = Net::HTTP.get(uri)
puts response
```
यहाँ, हम `require` के द्वारा net/http लाइब्रेरी को लोड करते हैं और एक यूआरआई का उपयोग करके एक HTTP अनुरोध भेजते हैं। उत्तर उस साइट के साथ संबंधित डाटा प्रदर्शित करता है।

## गहराई में जाएं:
HTTP का उदभव 1991 में टिम बर्नर्स-ली द्वारा किया गया था। यह अनुरोध भेजने की एक जटिल प्रक्रिया है जिसमें कई साधनों का प्रयोग किया जाता है। कुछ अल्टर्नेटिव्स के रूप में, HTTP अनुरोध भेजने का प्रयोग करने के लिए आप कुछ और लाइब्रेरी का भी प्रयोग कर सकते हैं, जैसे कि [Typhoeus](https://github.com/typhoeus/typhoeus) या [Faraday](https://github.com/lostisland/faraday)। HTTP अनुरोध भेजने के लिए, आमतौर पर `Net::HTTP` की जगह कुछ और क्लास का प्रयोग किया जाता है।

## इसके अलावा देखें:
- [Ruby-doc.org](https://ruby-doc.org/stdlib-2.7.2/libdoc/net/http/rdoc/Net/HTTP.html)
- [HTTP Request Methods](https://www.w3.org/Protocols/rfc2616/rfc2616-sec9.html)
- [HTTP Request and Response Basics](https://www.informit.com/articles/article.aspx?p=1831382&seqNum=2)