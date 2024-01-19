---
title:                "स्ट्रिंग का अंतर्कलन"
html_title:           "Arduino: स्ट्रिंग का अंतर्कलन"
simple_title:         "स्ट्रिंग का अंतर्कलन"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/interpolating-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
एक स्ट्रिंग इंटरपोलेट करना का मतलब होता है दूसरे स्ट्रिंग या वैरिएबल को एक स्ट्रिंग के बीच में जोड़ना। यह तब किया जाता है जब प्रोग्रामर को गणना की सामग्री को स्ट्रिंग के साथ संयोजित करना होता है।

## कैसे करें:
Gleam में, हम `string.concat` फ़ंक्शन का उपयोग करते हैं स्ट्रिंग इंटरपोलेशन के लिए।
```Gleam
let name = "Rahul"
let welcome_message = string.concat(["नमस्ते ", name, "!"])
```
इसका आउटपुट होगा `नमस्ते Rahul!`.

## गहरी जाँच:
1. ऐतिहासिक संदर्भ: पहले, प्रोग्रामर्स को कठिनाई होती थी स्ट्रिंग में डायनैमिक मूल्य डालने में और वे `+` ऑपरेटर का उपयोग करते थे। यह Gleam का बेहतर तरीका है।
2. विकल्प: विकल्प रूप में, `++` ऑपरेटर का भी उपयोग किया जा सकता है।
```Gleam
let welcome_message = "नमस्ते " ++ name ++ "!"
```
3. आंतरिक विवरण: `string.concat` फंकशन अन्य स्ट्रिंगों के साथ एक स्ट्रिंग को जोड़ने के लिए एक स्ट्रिंग सूची लेता है। यह स्ट्रिंग संचालन का प्रभावी तरीका है क्योंकि वह हीप अल्लोकेशन को कम करता है।

## यह भी देखें:
- Gleam String मैनुअल: [https://gleam.run/book/tour/strings.html](https://gleam.run/book/tour/strings.html)
- Gleam और स्ट्रिंग इंटरपोलेशन पर अधिक जानकारी: [https://gleam.run/news/gleam-v0.14-released/](https://gleam.run/news/gleam-v0.14-released/)