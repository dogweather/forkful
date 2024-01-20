---
title:                "एक वेब पेज डाउनलोड करना"
html_title:           "Kotlin: एक वेब पेज डाउनलोड करना"
simple_title:         "एक वेब पेज डाउनलोड करना"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

वेब पेज डाउनलोड करना का मतलब है इंटरनेट से उसकी फ़ाइल को लेना। प्रोग्रामर्स इसे डाटा खनन, टेस्टिंग, और मानिटरिंग के लिए करते हैं।

## कैसे करें:

वेब पेज को बाश में डाउनलोड करने के लिए `curl` कमांड शायद सबसे ज्यादा इस्तेमाल होता है। यहां एक उदाहरण है:

```Bash
curl -O https://www.example.com
```
यह कमांड 'example.com' वेब पेज को डाउनलोड करेगी और 'example.com' नामक फ़ाइल में सहेजेगी।

## गहरी जानकारी:

1. **ऐतिहासिक संदर्भ:** `curl` कमांड 1997 में शुरू हुई। जबकि Internet पर डेटा को ट्रांसफ़र करने के लिए पहले से ही कई प्रोटोकॉल मौजूद थे, लेकिन `curl` ने एक एकीकृत और आसान उपयोग की सुविधा दी।

2. **विकल्प:** वेब पेज डाउनलोड करने के लिए `wget` भी एक मशहूर टूल है, जिसे बाश में इस तरह से इस्तेमाल किया जाता है: `wget https://www.example.com`.

3. **विन्यास विवरण:** `curl` की बहुत सारी विकल्प हैं। उदाहरण के लिए `-L` ऑप्शन आपको अगर वेब पेज अन्य जगह पर रीडायरेक्ट हो जाता है, तो उसे खोजने की अनुमति देता है।

## अन्य जानकारी के लिए:

1. [`curl` कमांड का मैन पेज](https://www.man7.org/linux/man-pages/man1/curl.1.html)
2. [वेब पेज को `wget` के साथ डाउनलोड करने के लिए गाइड](https://www.tecmint.com/10-wget-command-examples-in-linux/)
3. [`curl` vs `wget`: एक तुलना](https://daniel.haxx.se/docs/curl-vs-wget.html)