---
date: 2024-01-20 17:45:28.936517-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Ruby \u092E\
  \u0947\u0902 \u0935\u0947\u092C \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\
  \u094B\u0921 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F `net/http`\
  \ \u0932\u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940 \u0915\u093E \u0907\u0938\
  \u094D\u0924\u0947\u092E\u093E\u0932 \u0939\u094B\u0924\u093E \u0939\u0948\u0964\
  \ \u0928\u0940\u091A\u0947 \u0915\u094B\u0921 \u092E\u0947\u0902 \u0926\u0947\u0916\
  \u093F\u090F."
lastmod: '2024-03-13T22:44:53.221885-06:00'
model: gpt-4-1106-preview
summary: "Ruby \u092E\u0947\u0902 \u0935\u0947\u092C \u092A\u0947\u091C \u0921\u093E\
  \u0909\u0928\u0932\u094B\u0921 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\
  \u090F `net/http` \u0932\u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940 \u0915\u093E\
  \ \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0939\u094B\u0924\u093E \u0939\
  \u0948\u0964 \u0928\u0940\u091A\u0947 \u0915\u094B\u0921 \u092E\u0947\u0902 \u0926\
  \u0947\u0916\u093F\u090F."
title: "\u0935\u0947\u092C \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\u094B\
  \u0921 \u0915\u0930\u0928\u093E"
weight: 42
---

## How to: (कैसे करें:)
Ruby में वेब पेज डाउनलोड करने के लिए `net/http` लाइब्रेरी का इस्तेमाल होता है। नीचे कोड में देखिए:

```ruby
require 'net/http'
require 'uri'

url = URI.parse('http://www.example.com/index.html')
response = Net::HTTP.get_response(url)

if response.is_a?(Net::HTTPSuccess)
  File.write('downloaded_page.html', response.body)
  puts "Web page downloaded successfully!"
else
  puts "Error downloading web page."
end
```

अगर सफल रहा, तो आउटपुट होगा:

```
Web page downloaded successfully!
```

## Deep Dive (गहराई से जानकारी):
Ruby में किसी वेब पेज को डाउनलोड करने का काम `net/http` लाइब्रेरी से शुरु हुआ, जो कि HTTP प्रोटोकॉल्स को संभालती है। ये प्रोटोकॉल HTML और दूसरे वेब रिसोर्सेज को ट्रांसफर करने के लिए इंटरनेट पर मानक तरीका है। बदलते समय के साथ, कई अन्य लाइब्रेरीज जैसे कि `open-uri` और `mechanize` भी आई हैं, जो कुछ ज्यादा फंक्शनलिटी प्रोवाइड करती हैं।

इन लाइब्रेरीज के इस्तेमाल से आप कोड में बस कुछ ही पंक्तियों का इस्तेमाल करके वेब पेज के डाटा को प्रोसेस कर सकते हैं, जिससे कि टाइम और एफर्ट की बचत होती है।

## See Also (देखें ये भी):
- [Ruby डॉक्यूमेंटेशन Net::HTTP](https://ruby-doc.org/stdlib-3.0.0/libdoc/net/http/rdoc/Net/HTTP.html)
- [Open-URI](https://ruby-doc.org/stdlib-3.0.0/libdoc/open-uri/rdoc/OpenURI.html)
- [Nokogiri जेम (HTML/XML डाटा को पार्स करने के लिए)](https://nokogiri.org/)
- [Mechanize जेम (एक ऑटोमेशन लाइब्रेरी जो वेब इंटरैक्शन को सिम्पलीफाई करती है)](https://github.com/sparklemotion/mechanize)
