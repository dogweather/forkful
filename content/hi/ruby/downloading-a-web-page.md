---
title:                "वेब पेज डाउनलोड करना"
date:                  2024-01-20T17:45:28.936517-07:00
model:                 gpt-4-1106-preview
simple_title:         "वेब पेज डाउनलोड करना"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
वेब पेज को डाउनलोड करना मतलब उसका HTML कोड अपने कंप्यूटर पर सेव करना है। कोडर्स यह इसलिए करते हैं ताकि वेब डाटा के साथ ऑफलाइन काम कर सकें, जैसे की डाटा पार्सिंग और वेब स्क्रेपिंग।

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
