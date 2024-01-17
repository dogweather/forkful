---
title:                "एक वेब पेज डाउनलोड करना"
html_title:           "Gleam: एक वेब पेज डाउनलोड करना"
simple_title:         "एक वेब पेज डाउनलोड करना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

# Web Page को डाउनलोड क्यों किया जाता है? 

डाउनलोड का अर्थ है, कोई वेब पेज को अपनी मशीन पर स्थानांतरित करना। प्रोग्रामर वेब पेज डाउनलोड करते हैं ताकि वे कंप्यूटर चलाने के दौरान उन्हें इस्तेमाल कर सकें। 

# कैसे करें: 

```Gleam 
let पेज = http.डाउनलोड("https://google.com")
पेज.लोड()
```

आप सीधे अपने Gleam कोड में `http` लाइब्रेरी का इस्तेमाल करके पेज को डाउनलोड कर सकते हैं। आप डाउनलोड किए गए पेज को `load()` फंक्शन को इस्तेमाल करके लोड कर सकते हैं। 

# गहराई में जाएँ: 

पहले से ही `HTTP` सेवा के लिए कई अन्य विकल्प हैं, लेकिन हमने `HTTP` सेवा को विकसित करते समय बहुत सारे समस्याओं को दूर किया है। आप आसानी से `HTTP` सेवा का प्रयोग करके वेब पेज को डाउनलोड कर सकते हैं। 

# देखें भी:

- [मूल ग्लीम दस्तावेज़] (https://gleam.run/documentation.html)
- [HTTP सेवा के बारे में संपूर्ण जानकारी] (https://github.com/gleam-lang/http)
- [वेब पेज डाउनलोड करने के लिए पाथ्रों की जाँच] (https://github.com/gleam-lang/path)