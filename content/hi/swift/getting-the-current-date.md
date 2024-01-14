---
title:                "Swift: वर्तमान दिनांक प्राप्त करना"
programming_language: "Swift"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/swift/getting-the-current-date.md"
---

{{< edit_this_page >}}

## क्यों
आजकल स्विफ्ट प्रोग्रामिंग भाषा एक बहुत ही लोकप्रिय और प्रभावी भाषा है। इसमें दिनांक को प्राप्त करने के लिए समय और समय जानकारी को प्राप्त करने के लिए बहुत कुछ है। स्विफ्ट दिनांक को पाने के लिए खुले स्रोत कोड (Open Source code) को साझा कर सकते हैं। यह लोगों को अपनी एप्लिकेशन्स में उपयोग का दौर को आसान करता है।

## कैसे करें
```Swift
let date = Date()
let formatter = DateFormatter()
formatter.dateFormat = "dd.MM.yyyy"
let result = formatter.string(from: date)
print(result)
```
यहां, हमने दिनांक को कैसे प्राप्त कर सकते हैं वह दिखाया है। पहले, हमने "Date" क्लास का उपयोग करके वर्तमान तारीख को एक प्रतिनिधि अभिव्यक्ति बनाई है। फिर, हमने "DateFormatter" क्लास का उपयोग करके एक तारीख प्रारूप (format) निर्धारित किया है। अंत में, हमने "string(from:)" फंक्शन का उपयोग करके वर्तमान तारीख को तारीख प्रारूप में परिणाम के रूप में प्रिंट किया है।

## गहराई तक जानें
स्विफ्ट में तारीख को प्राप्त करने के लिए और विभिन्न तारीख प्रारूपों को उपयोग करने के लिए अनेक विकल्प हैं। आप "DateComponents", "Calendar" और "TimeZone" जैसे अन्य क्लासों का भी उपयोग कर सकते हैं। आप इनके बारे में अधिक जानकारी के लिए [स्विफ्ट दस्तावेज़](https://swift.org/documentation) पर जा सकते हैं।

## देखें
[स्विफ्ट विकिपीडिया](https://hi.wikipedia.org/wiki/स्विफ्ट_(प्रोग्रामिंग_भाषा)) |
[ट्यूटोरियल वीडियो](https://www.youtube.com/watch?v=_Kh7jIiUU0w)