---
date: 2024-01-20 18:03:07.977295-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902? (How to:) HTTP \u092C\
  \u0947\u0938\u093F\u0915 \u092A\u094D\u0930\u092E\u093E\u0923\u0940\u0915\u0930\u0923\
  \ \u092E\u0947\u0902 'Authorization' \u0939\u0948\u0921\u0930 \u0915\u093E \u0907\
  \u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0939\u094B\u0924\u093E \u0939\u0948\
  , \u091C\u094B \u092C\u0947\u0938-64 \u0915\u094B\u0921\u093F\u0902\u0917 \u0915\
  \u093E \u092A\u094D\u0930\u092F\u094B\u0917 \u0915\u0930\u0924\u093E \u0939\u0948\
  \u0964 \u092F\u0939 \u0924\u0915\u0928\u0940\u0915 \u0935\u0947\u092C \u0915\u0940\
  \ \u0936\u0941\u0930\u0941\u0906\u0924 \u0915\u0947\u2026"
lastmod: '2024-04-05T22:51:07.898936-06:00'
model: gpt-4-1106-preview
summary: ") HTTP \u092C\u0947\u0938\u093F\u0915 \u092A\u094D\u0930\u092E\u093E\u0923\
  \u0940\u0915\u0930\u0923 \u092E\u0947\u0902 'Authorization' \u0939\u0948\u0921\u0930\
  \ \u0915\u093E \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0939\u094B\u0924\
  \u093E \u0939\u0948, \u091C\u094B \u092C\u0947\u0938-64 \u0915\u094B\u0921\u093F\
  \u0902\u0917 \u0915\u093E \u092A\u094D\u0930\u092F\u094B\u0917 \u0915\u0930\u0924\
  \u093E \u0939\u0948\u0964 \u092F\u0939 \u0924\u0915\u0928\u0940\u0915 \u0935\u0947\
  \u092C \u0915\u0940 \u0936\u0941\u0930\u0941\u0906\u0924 \u0915\u0947 \u0926\u093F\
  \u0928\u094B\u0902 \u0938\u0947 \u0939\u0948 \u0914\u0930 \u0905\u092D\u0940 \u092D\
  \u0940 \u0938\u0930\u0932 \u0914\u0930 \u092B\u091F\u093E\u092B\u091F \u0911\u0925\
  \u0947\u0902\u091F\u093F\u0915\u0947\u0936\u0928 \u0915\u0947 \u0932\u093F\u090F\
  \ \u092A\u094D\u0930\u092F\u094B\u0917 \u0939\u094B\u0924\u0940 \u0939\u0948\u0964\
  \ \u0935\u0948\u0938\u0947 \u0924\u094B \u092F\u0939 \u092C\u0939\u0941\u0924 \u0938\
  \u093F\u0915\u094D\u092F\u094B\u0930 \u0928\u0939\u0940\u0902 \u0939\u0948, \u0907\
  \u0938\u0932\u093F\u090F \u0906\u092E\u0924\u094C\u0930 \u092A\u0930 \u0907\u0938\
  \u0947 SSL/TLS \u0915\u0947 \u0938\u093E\u0925 \u0907\u0938\u094D\u0924\u0947\u092E\
  \u093E\u0932 \u0915\u093F\u092F\u093E \u091C\u093E\u0924\u093E \u0939\u0948\u0964\
  \ \u0907\u0938\u0915\u0947 \u0905\u0932\u093E\u0935\u093E, \u091C\u094D\u092F\u093E\
  \u0926\u093E \u0938\u0941\u0930\u0915\u094D\u0937\u093F\u0924 \u0935\u093F\u0915\
  \u0932\u094D\u092A \u091C\u0948\u0938\u0947 \u0915\u093F OAuth, Digest \u092A\u094D\
  \u0930\u092E\u093E\u0923\u0940\u0915\u0930\u0923 \u0914\u0930 API \u0915\u0940\u091C\
  \ \u092D\u0940 \u092E\u094C\u091C\u0942\u0926 \u0939\u0948\u0902\u0964."
title: "\u092C\u0947\u0938\u093F\u0915 \u092A\u094D\u0930\u092E\u093E\u0923\u0940\u0915\
  \u0930\u0923 \u0915\u0947 \u0938\u093E\u0925 HTTP \u0905\u0928\u0941\u0930\u094B\
  \u0927 \u092D\u0947\u091C\u0928\u093E"
weight: 45
---

## कैसे करें? (How to:)
```Ruby
require 'net/http'
require 'uri'

uri = URI('http://example.com/posts')
username = 'me'
password = 'mypassword'

Net::HTTP.start(uri.host, uri.port) do |http|
  request = Net::HTTP::Get.new(uri)
  request.basic_auth(username, password)

  response = http.request(request)
  puts response.body
end
```

सैंपल आउटपुट:
```
[{"id": 1, "title": "हेल्लो वर्ल्ड!", "description": "यह एक परिचय है..."}, ...और रिकॉर्ड्स...]
```

## विस्तार से जानकारी (Deep Dive)
HTTP बेसिक प्रमाणीकरण में 'Authorization' हैडर का इस्तेमाल होता है, जो बेस-64 कोडिंग का प्रयोग करता है। यह तकनीक वेब की शुरुआत के दिनों से है और अभी भी सरल और फटाफट ऑथेंटिकेशन के लिए प्रयोग होती है। वैसे तो यह बहुत सिक्योर नहीं है, इसलिए आमतौर पर इसे SSL/TLS के साथ इस्तेमाल किया जाता है। इसके अलावा, ज्यादा सुरक्षित विकल्प जैसे कि OAuth, Digest प्रमाणीकरण और API कीज भी मौजूद हैं।

## संबंधित स्रोत (See Also)
- Ruby's Net::HTTP documentation: [Ruby-Doc.org](https://ruby-doc.org/stdlib-2.7.1/libdoc/net/http/rdoc/Net/HTTP.html)
- Base64 encoding in Ruby: [apidock.com](https://apidock.com/ruby/Base64)
- More about HTTP Basic Authentication: [MDN Web Docs](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
