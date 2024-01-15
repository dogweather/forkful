---
title:                "एक वेब पेज को डाउनलोड करना"
html_title:           "Ruby: एक वेब पेज को डाउनलोड करना"
simple_title:         "एक वेब पेज को डाउनलोड करना"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Why
वेब पेज को डाउनलोड करने में हमारा उद्देश्य वह डाउनलोड को अपने स्वयं के उपयोग और प्रशिक्षण को पहचानने के लिए है। 

## How To
```Ruby
require 'open-uri'

# वेब पेज को डाउनलोड करने के लिए लिंक को दर्ज करें
url = "https://www.example.com"

# ओपन-यूआरआई को एक वेब प्रोटोकॉल ऑब्जेक्ट में कनवर्ट करें
page = URI.open(url)

# पेज के भीतरी सामग्री को पाने के लिए रेड ज़ोन का उपयोग करें 
# पैदाल दृश्य को प्रिंट करने के लिए केवल प्रेरण जोड़ें
puts page.read
```

आउटपुट:
```
<!DOCTYPE HTML>
<html>
<head>
<title>उदाहरण वेबसाइट</title>
</head>
<body>
<h1>नमस्ते दुनिया</h1>
<p>यह हमारा उदाहरण वेबसाइट है।</p>
</body>
</html>
```

## Deep Dive
डाउनलोड वेब पेज प्रोसेस में, हम वेब प्रोटोकॉल का उपयोग करते हैं जो हमें डाउनलोड करने के लिए प्रदान किए जाने वाले रिसोर्सेस को पहुंच और प्रदान करता है। इसके अलावा, हम अन्य Ruby gems जैसे html-parser का भी उपयोग कर सकते हैं जो हमें वेब पेज के साथ साथ उसकी सामग्री को भी प्रोसेस करने में मदद करते हैं। 

## See Also
- [Net/HTTP](https://ruby-doc.org/stdlib-2.7.1/libdoc/net/http/rdoc/index.html)
- [HTML-Parser](https://github.com/sparklemotion/nokogiri)