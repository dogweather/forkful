---
title:                "स्ट्रिंग अंतःस्थापन करना"
html_title:           "Haskell: स्ट्रिंग अंतःस्थापन करना"
simple_title:         "स्ट्रिंग अंतःस्थापन करना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/interpolating-a-string.md"
---

{{< edit_this_page >}}

# क्या और क्यों?

फसला में एक स्ट्रिंग (string) को समेकित करना क्या है और इसे प्रोग्रामर्स ऐसा क्यों करते हैं? फसला, स्ट्रिंग्स को इंटरपोलेट करने का एक आसान तरीका प्रदान करता है। इससे आप अपने स्ट्रिंग में परिवर्तनीय अंश (variables) शामिल कर सकते हैं जो कि प्रोग्राम के चलते परिवर्तित हो सकते हैं।

# कैसे करें?

```Haskell
name = "आप"
age = 25
greeting = "नमस्ते, {name}! आपकी उम्र {age} साल है।"
putStrLn greeting
```

आपको यह आउटपुट मिलेगा:

```
नमस्ते, आप! आपकी उम्र 25 साल है।
```

आप स्ट्रिंग में अपनी चाहे जितनी भी परिवर्तनीय अंश जोड़ सकते हैं और प्रोग्राम चलते समय उन्हें बदल सकते हैं। आप `++` ऑपरेटर का उपयोग प्रत्येक परिवर्तनीय अंश को स्ट्रिंग में जोड़ने के लिए कर सकते हैं।

# गहराई में जाएं

स्ट्रिंग इंटरपोलेशन को पहले से ही पैदावार परिस्थितियों के लिए जाना जाता है, लेकिन फसला ने इसे हाल ही में फंक्शनल प्रोग्रामिंग की विद्वत्ता से भरपूर कर दिया है। अन्य विकल्पों में पाइथन के `f-strings`, जावा में `string.format()` और जावास्क्रिप्ट में `${}` टेम्पलेट स्ट्रिंग हैं। फसला में इंटरपोलेशन का विषय करते समय, हम ज्यादातर अपने कोड को साफ (clean) और एकांकित (concise) रखने पर ध्यान देते हैं।

# और भी देखें

आप `string-conv` पैकेज में फसला के साथ एकदम लात हो सकते हैं। यह पैकेज फसला से स्ट्रिंग्स को परिवर्तित करने के लिए अतिरिक्त फंक्शन और ऑपरेशन शामिल करता है। आप [फसला के विभिन्न कन्वर्टर्स](https://hackage.haskell.org/package/string-conv-0.1.1.0/docs/Fossa.html) का उपयोग देख सकते हैं।