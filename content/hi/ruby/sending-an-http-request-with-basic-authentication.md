---
title:                "बेसिक प्रमाणीकरण के साथ HTTP अनुरोध भेजना"
date:                  2024-01-20T18:03:07.977295-07:00
model:                 gpt-4-1106-preview
simple_title:         "बेसिक प्रमाणीकरण के साथ HTTP अनुरोध भेजना"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)

HTTP अनुरोध जब बेसिक प्रमाणीकरण के साथ भेजा जाता है, तब प्रोग्राम एक सुरक्षित तरीके से यूज़रनेम और पासवर्ड का इस्तेमाल करके सर्वर से डेटा का आदान-प्रदान करता है। डेवलपर्स इस प्रक्रिया का इस्तेमाल तब करते हैं जब वे सिक्योर एपीआई या वेब सर्विसेज़ से जुड़ना चाहते हों।

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
