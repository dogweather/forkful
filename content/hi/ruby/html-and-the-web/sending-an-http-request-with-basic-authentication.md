---
date: 2024-01-20 18:03:07.977295-07:00
description: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u091C\u092C \u092C\u0947\u0938\
  \u093F\u0915 \u092A\u094D\u0930\u092E\u093E\u0923\u0940\u0915\u0930\u0923 \u0915\
  \u0947 \u0938\u093E\u0925 \u092D\u0947\u091C\u093E \u091C\u093E\u0924\u093E \u0939\
  \u0948, \u0924\u092C \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E \u090F\
  \u0915 \u0938\u0941\u0930\u0915\u094D\u0937\u093F\u0924 \u0924\u0930\u0940\u0915\
  \u0947 \u0938\u0947 \u092F\u0942\u091C\u093C\u0930\u0928\u0947\u092E \u0914\u0930\
  \ \u092A\u093E\u0938\u0935\u0930\u094D\u0921 \u0915\u093E \u0907\u0938\u094D\u0924\
  \u0947\u092E\u093E\u0932 \u0915\u0930\u0915\u0947 \u0938\u0930\u094D\u0935\u0930\
  \ \u0938\u0947 \u0921\u0947\u091F\u093E \u0915\u093E \u0906\u0926\u093E\u0928-\u092A\
  \u094D\u0930\u0926\u093E\u0928\u2026"
lastmod: '2024-03-13T22:44:53.223575-06:00'
model: gpt-4-1106-preview
summary: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u091C\u092C \u092C\u0947\u0938\
  \u093F\u0915 \u092A\u094D\u0930\u092E\u093E\u0923\u0940\u0915\u0930\u0923 \u0915\
  \u0947 \u0938\u093E\u0925 \u092D\u0947\u091C\u093E \u091C\u093E\u0924\u093E \u0939\
  \u0948, \u0924\u092C \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E \u090F\
  \u0915 \u0938\u0941\u0930\u0915\u094D\u0937\u093F\u0924 \u0924\u0930\u0940\u0915\
  \u0947 \u0938\u0947 \u092F\u0942\u091C\u093C\u0930\u0928\u0947\u092E \u0914\u0930\
  \ \u092A\u093E\u0938\u0935\u0930\u094D\u0921 \u0915\u093E \u0907\u0938\u094D\u0924\
  \u0947\u092E\u093E\u0932 \u0915\u0930\u0915\u0947 \u0938\u0930\u094D\u0935\u0930\
  \ \u0938\u0947 \u0921\u0947\u091F\u093E \u0915\u093E \u0906\u0926\u093E\u0928-\u092A\
  \u094D\u0930\u0926\u093E\u0928 \u0915\u0930\u0924\u093E \u0939\u0948\u0964 \u0921\
  \u0947\u0935\u0932\u092A\u0930\u094D\u0938 \u0907\u0938 \u092A\u094D\u0930\u0915\
  \u094D\u0930\u093F\u092F\u093E \u0915\u093E \u0907\u0938\u094D\u0924\u0947\u092E\
  \u093E\u0932 \u0924\u092C \u0915\u0930\u0924\u0947 \u0939\u0948\u0902 \u091C\u092C\
  \ \u0935\u0947 \u0938\u093F\u0915\u094D\u092F\u094B\u0930 \u090F\u092A\u0940\u0906\
  \u0908 \u092F\u093E \u0935\u0947\u092C \u0938\u0930\u094D\u0935\u093F\u0938\u0947\
  \u091C\u093C \u0938\u0947 \u091C\u0941\u0921\u093C\u0928\u093E \u091A\u093E\u0939\
  \u0924\u0947 \u0939\u094B\u0902\u0964."
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
