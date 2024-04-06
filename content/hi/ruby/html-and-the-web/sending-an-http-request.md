---
date: 2024-01-20 18:00:51.977089-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902) Ruby \u092E\
  \u0947\u0902 HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u0928\u0947\
  \ \u0915\u0947 \u0932\u093F\u090F `net/http` \u0938\u094D\u091F\u0948\u0902\u0921\
  \u0930\u094D\u0921 \u0932\u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940 \u0915\
  \u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0947\u0902."
lastmod: '2024-04-05T21:53:55.177345-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902) Ruby \u092E\u0947\u0902\
  \ HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u0928\u0947 \u0915\
  \u0947 \u0932\u093F\u090F `net/http` \u0938\u094D\u091F\u0948\u0902\u0921\u0930\u094D\
  \u0921 \u0932\u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940 \u0915\u093E \u0909\
  \u092A\u092F\u094B\u0917 \u0915\u0930\u0947\u0902."
title: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u0928\u093E"
weight: 44
---

## How to: (कैसे करें)
Ruby में HTTP अनुरोध भेजने के लिए `net/http` स्टैंडर्ड लाइब्रेरी का उपयोग करें:
```ruby
require 'net/http'
require 'uri'

uri = URI('http://example.com/index.html')
response = Net::HTTP.get_response(uri)

puts response.code # सर्वर से प्रतिक्रिया कोड
puts response.body # सर्वर से आई सामग्री
```
सैंपल आउटपुट:
```
200
<!doctype html>...
```

## Deep Dive (गहराई से जानकारी)
आरंभ में, HTTP अनुरोध केवल सरल टेक्स्ट डेटा के आदान-प्रदान के लिए थे। अब, जटिल JSON या XML जैसे फॉर्मेट में भी डेटा भेजा और प्राप्त किया जाता है। Ruby में `net/http` के अलावा, `httparty` और `rest-client` जैसे जेम्स भी हैं जो HTTP अनुरोध को और सरल बनाते हैं। इम्प्लीमेंटेशन में एसिंक्रोनस ऑपरेशन, थ्रेडिंग और सेशन मैनेजमेंट जैसी जटिलताएँ भी शामिल हो सकती हैं।

## See Also (और भी देखें)
- Ruby's `net/http` documentation: [Ruby-Doc.org - Net::HTTP](https://ruby-doc.org/stdlib-3.0.0/libdoc/net/http/rdoc/Net/HTTP.html)
- `httparty` gem: [GitHub - httparty/httparty](https://github.com/jnunemaker/httparty)
- `rest-client` gem: [GitHub - rest-client/rest-client](https://github.com/rest-client/rest-client)
- More on HTTP requests in Ruby: [RubyGuides - HTTP Requests](https://www.rubyguides.com/2018/08/ruby-http-request/)
