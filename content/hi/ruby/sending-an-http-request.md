---
title:                "HTTP अनुरोध भेजना"
aliases:
- hi/ruby/sending-an-http-request.md
date:                  2024-01-20T18:00:51.977089-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTP अनुरोध भेजना"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/sending-an-http-request.md"
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
