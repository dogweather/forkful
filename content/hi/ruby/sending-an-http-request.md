---
title:                "Ruby: Http अनुरोध भेजना"
simple_title:         "Http अनुरोध भेजना"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/sending-an-http-request.md"
---

{{< edit_this_page >}}

## क्यों

HTTP अनुरोध भेजने में क्यों कोई रुचि रखे?

वेब डिवेलपमेंट में, हम अक्सर दूरस्थ सर्वर से डेटा को प्राप्त करने के लिए एक अनुरोध भेजते हैं। इस अनुरोध को HTTP अनुरोध कहा जाता है और यही अनुरोध हमारी ब्राउज़र और वेबसाइट के बीच डेटा का आदान-प्रदान करने का माध्यम होता है। इसलिए, यह अनुरोध भेजना वेब डिवेलपमेंट के लिए बहुत महत्वपूर्ण है।

## कैसे करें

अब हम रूबी में HTTP अनुरोध भेजने के लिए कैसे कोड लिखेंगे? चलिए इसे एक उदाहरण के साथ देखते हैं।

```Ruby
require 'net/http'
 
# अनुरोध भेजने के लिए URI बनाएं
uri = URI('https://www.example.com')

# अनुरोध के माध्यम से डेटा प्राप्त करें
response = Net::HTTP.get(uri)

# डेटा की प्रिंट करें
puts response
```

उपरोक्त कोड के अनुसार, हमने `net/http` लायब्रेरी को इंपोर्ट किया और URI के जरिए अपने अनुरोध के लिए एक लिंक तैयार किया। उसके बाद, हम `Net::HTTP.get` को उस URI पर डेटा को प्राप्त करने के लिए बोले। अंत में, हम उस डेटा को प्रिंट करते हैं। नीचे दिए गए उदाहरण की तरह आपको भी एक HTTP अनुरोध करने के बाद डेटा की प्रिंट मिलेगी:

```
<!DOCTYPE html>
<html>
<head>
<title>Example Domain</title>
</head>
<body>
<h1>Example Domain</h1>
<p>This domain is for use in illustrative examples in documents. You may use this
domain in literature without prior coordination or asking for permission.</p>
<p><a href="https://www.iana.org/domains/example">More information...</a></p>
</body>
</html>
```

## गहराई में जाएं

अब आपने एक बहुत सरल उदाहरण देखा है, चलिए इस अनुरोध को गहराई से सम