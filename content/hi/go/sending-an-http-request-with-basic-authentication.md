---
title:                "बुनियादी प्रमाणीकरण के साथ एक http अनुरोध भेजना"
html_title:           "Go: बुनियादी प्रमाणीकरण के साथ एक http अनुरोध भेजना"
simple_title:         "बुनियादी प्रमाणीकरण के साथ एक http अनुरोध भेजना"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्यों

आज के समय में डेटा सुरक्षा बहुत महत्वपूर्ण है और इसलिए अधिकांश ऑनलाइन सेवाओं को उपयोगकर्ताओं का ऑथेंटिकेशन करने के लिए HTTP अनुरोधों में बेसिक ऑथेंटिकेशन का उपयोग किया जाता है। इस सेक्शन में हम जानेंगे कि बेसिक ऑथेंटिकेशन के साथ HTTP अनुरोध कैसे भेजा जाता है।

## कैसे करें

```Go
req, err := http.NewRequest("GET", "https://www.example.com", nil)
if err != nil {
    log.Fatal(err)
}

req.SetBasicAuth("username", "password")

resp, err := http.DefaultClient.Do(req)
if err != nil {
    log.Fatal(err)
}

defer resp.Body.Close()

fmt.Println(resp.StatusCode)
```

ऊपर दिए गए कोड की मदद से हम दिए गए यूआरएल तक GET अनुरोध भेजते हैं। इसमें हमने `http.NewRequest()` फ़ंक्शन का उपयोग करके नया अनुरोध बनाया है। फिर हमने `req.SetBasicAuth()` के साथ उपयोगकर्ता का नाम और पासवर्ड दिया है। अंत में `http.DefaultClient.Do()` फ़ंक्शन के द्वारा हम अनुरोध को भेजते हैं और उसका प्रतिक्रिया कोड द्वारा प्रिंट किया जाता है।

## गहराई में जानें

HTTP अनुरोधों में बेसिक ऑथेंटिकेशन के साथ उपयोगकर्ता के नाम और पासवर्ड एक आम मूल्य होते हैं जो कि एक base64 encoded string के रूप में अनुरोध में शामिल होते हैं। एक अलग header में यह जानकारी भेजी जाती है `"Authorization: Basic <base64 encoded string>"`। हमने `http.NewRequest()` के साथ उपयोगकर्ता के नाम और पासवर्ड को सेट करने के लिए `req.SetBasicAuth()` फ़ंक्शन का उपयोग किया है। इससे हमारे द्वारा बनाए गए नए अनुरोध में यह header भी शामिल हो जाता है औ