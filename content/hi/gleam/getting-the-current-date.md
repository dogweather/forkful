---
title:    "Gleam: वर्तमान दिनांक प्राप्त करना"
keywords: ["Gleam"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/getting-the-current-date.md"
---

{{< edit_this_page >}}

## क्यों:
 कोई व्यक्ति वर्तमान तारीख प्राप्त करने में सक्रिय होने के लिए *क्यों* आगे बढ़ेगा।

## कैसे करें:
 ```Gleam
 import gleam/time

 // वर्तमान तारीख लेने के लिए गॉल-अप फ़ंक्शन का उपयोग करें
 let current_date = time.now()

 // तारीख को अनुक्रमणिक तारीख में परिवर्तित करें
 let formatted_date = time.format(current_date, "%Y-%m-%d")

 // प्रायोगिक उत्पाद:
 // 2021-01-23
 ```

## गहराई में उतरें:
वर्तमान तारीख प्राप्त करना बेहद आसान है। आप विभिन्न तारीख स्थापित करने के लिए *लेक्षणियता* तारीख के साथ समय फ़ंक्शन का उपयोग कर सकते हैं। इसके अलावा, आप तारीख को इच्छित ढंग से प्रारूपित करने के लिए स्थिर खाका खोज सकते हैं जो आपको अपनी जरूरतों के अनुसार उपयोग करने की अनुमति देता है।

## देखें भी:
[गॉल-अप तिथि और समय फ़ंक्शन](https://gleam.run/lib/gleam/time.html#Time.Gregorian)
[गॉल-अप कन्ट्रोल स्ट्रक्टर](https://gleam.run/language/structs.html#control-structs)
[गॉल-अप स्ट्रिंग्स फ़ॉर्मेटिंग](https://gleam.run/lib/gleam/string.html#String1.format)