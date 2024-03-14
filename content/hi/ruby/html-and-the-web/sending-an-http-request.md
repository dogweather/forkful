---
date: 2024-01-20 18:00:51.977089-07:00
description: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u0928\u093E\
  \ \u092F\u093E\u0928\u0940 \u0935\u0947\u092C \u0938\u0930\u094D\u0935\u0930 \u0938\
  \u0947 \u091C\u093E\u0928\u0915\u093E\u0930\u0940 \u092E\u093E\u0902\u0917\u0928\
  \u093E\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938\
  \ \u0910\u0938\u093E \u0921\u0947\u091F\u093E \u0939\u093E\u0938\u093F\u0932 \u0915\
  \u0930\u0928\u0947, \u092B\u0949\u0930\u094D\u092E \u092D\u0930\u0928\u0947, \u092F\
  \u093E APIs \u0915\u0947 \u0938\u093E\u0925 \u092C\u093E\u0924\u091A\u0940\u0924\
  \ \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u0915\u0930\u0924\u0947\
  \ \u0939\u0948\u0902\u0964"
lastmod: '2024-03-13T22:44:53.218610-06:00'
model: gpt-4-1106-preview
summary: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u0928\u093E\
  \ \u092F\u093E\u0928\u0940 \u0935\u0947\u092C \u0938\u0930\u094D\u0935\u0930 \u0938\
  \u0947 \u091C\u093E\u0928\u0915\u093E\u0930\u0940 \u092E\u093E\u0902\u0917\u0928\
  \u093E\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938\
  \ \u0910\u0938\u093E \u0921\u0947\u091F\u093E \u0939\u093E\u0938\u093F\u0932 \u0915\
  \u0930\u0928\u0947, \u092B\u0949\u0930\u094D\u092E \u092D\u0930\u0928\u0947, \u092F\
  \u093E APIs \u0915\u0947 \u0938\u093E\u0925 \u092C\u093E\u0924\u091A\u0940\u0924\
  \ \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u0915\u0930\u0924\u0947\
  \ \u0939\u0948\u0902\u0964"
title: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u0928\u093E"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
HTTP अनुरोध भेजना यानी वेब सर्वर से जानकारी मांगना। प्रोग्रामर्स ऐसा डेटा हासिल करने, फॉर्म भरने, या APIs के साथ बातचीत करने के लिए करते हैं।

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
