---
title:                "http अनुरोध भेजना"
html_title:           "Elixir: http अनुरोध भेजना"
simple_title:         "http अनुरोध भेजना"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

HTTP अनुरोध भेजने का मतलब सर्वर से जानकारी मांगना होता है। प्रोग्रामर्स इसे APIs से जानकारी ताजगी के लिए, या सर्वर से डाटा प्राप्त करने के लिए करते हैं।

## कैसे करें:

यहां Ruby में HTTP अनुरोध कैसे भेजें, उसका कोड है:

```Ruby
require 'net/http'
require 'uri'

uri = URI.parse("http://example.com/search")
response = Net::HTTP.get_response(uri)

puts response.body
```

आपको उत्तर में सर्वर से मिलने वाला डाटा मिलेगा।

## गहराई से जानें:

HTTP अनुरोध भेजने का तरीका 90 के दशक से मौजूद है और यह एक महत्वपूर्ण हिस्सा है कंप्यूटर संवाद का। Net::HTTP Ruby का एक स्टैंडर्ड लाइब्रेरी है जो इसका काम करता है, लेकिन अन्य libraries जैसे कि 'httparty' या 'rest-client' भी मौजूद हैं जो इसे और आसान बना सकते हैं।

## देखें भी:

* [Net::HTTP प्रलेखन](https://ruby-doc.org/stdlib-2.7.1/libdoc/net/http/rdoc/Net/HTTP.html)
* [Rest-Client प्रलेखन](https://www.rubydoc.info/gems/rest-client/frames)
* [एचटीटीपीपार्टी प्रलेखन](https://www.rubydoc.info/github/jnunemaker/httparty)