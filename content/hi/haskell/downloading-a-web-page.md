---
title:                "वेब पेज को डाउनलोड करना।"
html_title:           "Haskell: वेब पेज को डाउनलोड करना।"
simple_title:         "वेब पेज को डाउनलोड करना।"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
वेब पेज डाउनलोड करना वह कार्य है जो प्रोग्रामर करते हैं ताकि वे वेबसाइट से डेटा को अपने कंप्यूटर पर लाएं और उसे प्रोसेस कर सकें। यह डेटा हो सकता है कि उनकी एप्लिकेशन में उपयोग किया जाए या वेबसाइट के अपडेट्स को लोगों को देखने दिए जाएं। प्रोग्रामर वेब पेज डाउनलोड करते हैं क्योंकि वे अपने प्रोग्राम में नए फीचर्स जोड़ने के लिए वेबसाइट के साथ एक्सेस करना चाहते हैं या तो आसानी से डेटा को प्रोसेस कर सकें।

## कैसे:
`Haskell` कोड ब्लॉक्स के अंदर आप एक web page को डाउनलोड कर सकते हैं और उसके अंतर्गत दिए गए लिंक्स को भी अपने कंप्यूटर पर सहेज सकते हैं।
```Haskell
import Network.HTTP
main = do
  let url = "https://www.example.com"
  page <- simpleHTTP (getRequest url)
  content <- getResponseBody page
  putStrLn content
```
यहां, `url` वह वेब पेज है जिसे आप डाउनलोड करना चाहते हैं। कोड ब्लॉक्स के अंत में परीक्षण में, आप पेज के सामग्री को हस्ताक्षर निकाल सकते हैं।

## डीप डाइव:
अगर आप एक web page को डाउनलोड करना चाहते हैं, तो आपके पास कई विकल्प हैं जैसे कि `Network.HTTP` या `Network.HTTP.Conduit` पैकेज। `Network.HTTP` उस पैकेज का older version है जो historical context के पूछता है कि इसका development किस आधार पर हुआ। `Network.HTTP.Conduit` उस पैकेज का नया version है जो अधूरे काम को करने के लिए आसान interface प्रदान करता है और कई feature को support करता है जैसे कि HTTP/2. आपको किस version को उपयोग करना है, यह आपके टारगेट को डेटा को डाउनलोड करने में आसानी से सक्षम बनाता है।

## देखें भी:
- [Network.HTTP पैकेज](https://hackage.haskell.org/package/HTTP)
- [Network.HTTP.Conduit पैकेज](https://hackage.haskell.org/package/http-conduit)