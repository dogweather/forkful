---
title:                "बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
html_title:           "C#: बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
simple_title:         "बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

---

## क्या और क्यों?
HTTP अनुरोध के साथ बेसिक प्रमाणीकरण भेजने का मतलब क्या है? इसका मतलब है कि आप HTTP सर्वर से प्रतिसाद प्राप्त करने के लिए एक "अनुरोध" भेजने के साथ, एक क्रिप्टिक "क्रेडेंशियल" (आपकी पहचान का एक प्रकार) भी भेजते हैं। का उपयोग करके आपके अनुरोध को प्रमाणित करते हैं। कम्प्यूटर प्रोग्रामर्स इसे इसलिए करते हैं क्योंकि ये क्रेडेंशियल्स कई प्रकार की वेब सेवाओं के लिए उपयोगी होते हैं, जिनमें से अधिकांश HTTP के माध्यम से प्राप्त होते हैं।

## कैसे करें:
नीचे Ruby के कोड की उदाहरण दिए गए हैं:

```ruby
require 'net/http'
require 'uri'

uri = URI.parse("http://example.com/")
request = Net::HTTP::Get.new(uri)
request.basic_auth("user", "pass")

response = Net::HTTP.start(uri.hostname, uri.port) do |http|
  http.request(request)
end

puts response.body
```
आपका संदेश जो समर्पित करना चाहते हैं, उन्हें `"user"` और `"pass"` में बदलें। इस कोड को चलाने पर, आपको सर्वर से प्रत्युत्तर मिलेगा, जिसे आप ```put response.body``` के द्वारा देख सकते हैं।

## गहरा डाइव
HTTP प्रमाणीकरण की अनुरोध की प्रक्रिया का विकास 1990 के दशक के शुरुआती दिनों में हुआ था, जब इंटरनेट इंजनियरिंग टास्क फोर्स (IETF) ने इसे RFC 1945 के रूप में परिभाषित किया। औरतर्किक विधाएँ जैसे कि OAuth और डाइजेस्ट प्रमाणीकरण, HTTP प्रमाणीकरण की योजनाओं के विस्तार के रूप में आती हैं। Ruby का `Net::HTTP` कक्षा HTTP अनुरोधों और प्रतिसाद को प्रमाणित करने के लिए बेसिक और डाइजेस्ट प्रमाणीकरण का समर्थन करती हैं। 

## और देखें
[Hindi में Ruby मैन्युअल](https://www.ruby-lang.org/hi/documentation/)
[HTTP प्रमाणीकरण](https://tools.ietf.org/html/rfc7617)
[रूबी नेट एचटीटीपी डॉक्स](https://ruby-doc.org/stdlib-2.7.1/libdoc/net/http/rdoc/Net/HTTP.html)