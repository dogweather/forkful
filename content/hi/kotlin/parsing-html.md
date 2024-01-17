---
title:                "हैलो वर्ल्ड (Hello World)"
html_title:           "Kotlin: हैलो वर्ल्ड (Hello World)"
simple_title:         "हैलो वर्ल्ड (Hello World)"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/parsing-html.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
HTML पार्सिंग से क्या तात्पर्य है और क्यों प्रोग्रामर्स इसे करते हैं? HTML पार्सिंग का उपयोग वेब डेवलपमेंट में डाटा को प्रोसेस करने और दिखाने के लिए किया जाता है। प्रोग्रामर्स इसे वेबसाइट डिजाइन, वेब क्रॉलिंग और डाटा स्क्रैपिंग के लिए इस्तेमाल करते हैं।

## कैसे करें:
```Kotlin
val htmlString = "<html><body><h1>Welcome to my website!</h1></body></html>"
val doc = Jsoup.parse(htmlString)
val title = doc.select("h1").text()
println(title)
```
इस विशेष कोड स्निपेट में, हम एक HTML स्ट्रिंग को पार्स करते हैं और उसमें से हेडिंग 1 को प्रिंट करते हैं। यहां, हम Jsoup नामक एक लाइब्रेरी का उपयोग करते हैं जो कोट्लिन में HTML पार्सिंग को आसान बनाता है। आप अपनी वेबसाइट के लिए डाटा प्रोसेस करने के लिए इसे इस्तेमाल कर सकते हैं।

## डीप डाइव:
HTML पार्सिंग को भारत में राष्ट्रीय स्तर पर डिजाइन किया गया था। यह अधिकांश भाषाओं के लिए मानक बन गया है और डीएनए या एसजीएमएल क्षेत्र में एक कामगार इसका उपयोग नहीं करता है। जब यह वेबसाइट के अंचों और उनसे व्यापक वेबसाइटों के बीच स्थिर हुआ, Jsoup स्रोत से निरंतर इससे अधिक जटिल राष्ट्रीय स्तर का प्रस्ताव रखती है और ऋण स्टाइल से सुरक्षित रहती है।

## देखें भी:
- [Kotlin की आधिकारिक दस्तावेज़ीकरण](https://kotlinlang.org/docs/reference/)
- [सीएलआर ट्यूटोरियल: कोट्लिन में HTML पार्सिंग](https://codelabs.developers.google.com/codelabs/java-to-kotlin/index.html?index=..%2F..index#12)
- [Jsoup ओपन स्रोत लाइब्रेरी](https://jsoup.org/)