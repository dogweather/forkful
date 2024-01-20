---
title:                "एक वेब पेज डाउनलोड करना"
html_title:           "Kotlin: एक वेब पेज डाउनलोड करना"
simple_title:         "एक वेब पेज डाउनलोड करना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)

एक वेब पेज डाउनलोड करना मतलब किसी वेबसाइट की जानकारी को अपने कंप्यूटर पर स्थानांतरित करना है। प्रोग्रामर्स इसे डाटा माइनिंग या ऑफलाइन उपयोग के लिए करते हैं।

## कैसे: (How to)

निम्नलिखित कोड से आप एक वेब पेज का कंटेंट कोड कर सकते हैं:

```Kotlin
import java.net.URL

fun main(args: Array<String>) {
    
    val websiteContent = URL("http://example.com").readText()
    
    println(websiteContent)
}
```

यह कोड की आउटपुट सम्पल होगी:

```Kotlin
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
</html>
```

## गहरी जानकारी: (Deep Dive)

वेब पेज डाउनलोड करना प्रारंभ में ब्राउज़र्स द्वारा इंटरनेट जानकारी को देखने के लिए किया गया था। आज के समय में, यह स्क्रिप्टिंग भाषाओं के द्वारा सरलीकृत हो गया है। 

एक विकल्प `Jsoup` है, जो एक जावा लाइब्ररी है जिसे HTML से डाटा खींचने के लिए उपयोग किया जाता है। 

`URL().readText()` तकनीक `HttpURLConnection` का उपयोग करके सर्वर से डाटा प्राप्त करती है।

## और देखें: (See Also)

जावा और कोटलिन के LIbraries:
1. [Jsoup](https://jsoup.org/)
2. [OkHttp](https://square.github.io/okhttp/).

ये लिंक्स आपकी कोडिंग की ज़रूरतों को और अधिक पॉलिश कर सकते हैं।