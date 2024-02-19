---
aliases:
- /hi/kotlin/downloading-a-web-page/
date: 2024-01-20 17:45:03.190479-07:00
description: "\u0935\u0947\u092C \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\
  \u094B\u0921\u093F\u0902\u0917 \u0938\u0947 \u0924\u093E\u0924\u094D\u092A\u0930\
  \u094D\u092F \u0907\u0902\u091F\u0930\u0928\u0947\u091F \u0938\u0947 \u0915\u093F\
  \u0938\u0940 \u0935\u0947\u092C\u0938\u093E\u0907\u091F \u0915\u0947 \u092A\u0947\
  \u091C \u0915\u0940 \u0938\u093E\u092E\u0917\u094D\u0930\u0940 \u0915\u094B \u092A\
  \u094D\u0930\u093E\u092A\u094D\u0924 \u0915\u0930\u0928\u093E \u0939\u0948\u0964\
  \ \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0907\
  \u0938\u0947 \u0921\u0947\u091F\u093E \u0905\u0928\u093E\u0932\u093F\u0938\u093F\
  \u0938, \u0935\u0947\u092C \u0938\u094D\u0915\u094D\u0930\u0947\u092A\u093F\u0902\
  \u0917, \u092F\u093E \u0911\u092B\u0932\u093E\u0907\u0928\u2026"
lastmod: 2024-02-18 23:09:03.266834
model: gpt-4-1106-preview
summary: "\u0935\u0947\u092C \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\u094B\
  \u0921\u093F\u0902\u0917 \u0938\u0947 \u0924\u093E\u0924\u094D\u092A\u0930\u094D\
  \u092F \u0907\u0902\u091F\u0930\u0928\u0947\u091F \u0938\u0947 \u0915\u093F\u0938\
  \u0940 \u0935\u0947\u092C\u0938\u093E\u0907\u091F \u0915\u0947 \u092A\u0947\u091C\
  \ \u0915\u0940 \u0938\u093E\u092E\u0917\u094D\u0930\u0940 \u0915\u094B \u092A\u094D\
  \u0930\u093E\u092A\u094D\u0924 \u0915\u0930\u0928\u093E \u0939\u0948\u0964 \u092A\
  \u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0907\u0938\u0947\
  \ \u0921\u0947\u091F\u093E \u0905\u0928\u093E\u0932\u093F\u0938\u093F\u0938, \u0935\
  \u0947\u092C \u0938\u094D\u0915\u094D\u0930\u0947\u092A\u093F\u0902\u0917, \u092F\
  \u093E \u0911\u092B\u0932\u093E\u0907\u0928\u2026"
title: "\u0935\u0947\u092C \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\u094B\
  \u0921 \u0915\u0930\u0928\u093E"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
वेब पेज डाउनलोडिंग से तात्पर्य इंटरनेट से किसी वेबसाइट के पेज की सामग्री को प्राप्त करना है। प्रोग्रामर्स इसे डेटा अनालिसिस, वेब स्क्रेपिंग, या ऑफलाइन एक्सेस के उद्देश्य से करते हैं।

## कैसे करें? (How to:)
कोटलिन में वेब पेज डाउनलोड करने के लिए `HttpURLConnection` क्लास का उपयोग करते हैं:

```Kotlin
import java.io.BufferedReader
import java.io.InputStreamReader
import java.net.HttpURLConnection
import java.net.URL

fun downloadWebPage(url: String): String? {
    val urlObj = URL(url)
    with(urlObj.openConnection() as HttpURLConnection) {
        requestMethod = "GET" // HTTP GET request
        BufferedReader(InputStreamReader(inputStream)).use {
            return it.readText() // पठन और डाउनलोड किया हुआ डेटा वापस करना
        }
    }
}

fun main() {
    val content = downloadWebPage("http://www.example.com")
    println(content)
}
```

सैंपल आउटपुट:
```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
...
```

## गहराई से समझें (Deep Dive)
इतिहास संदर्भ में, वेब पेज डाउनलोड करने की तकनीकें वक्त के साथ विकसित हुई हैं। पहले सरल HTTP लाइब्रेरीज से शुरू होकर, आज हमारे पास जेटपैक कम्पोज़ जैसे उन्नत टूल्स हैं। डाउनलोडिंग के विकल्पों में `OkHttp`, `Retrofit` जैसी लाइब्रेरीज शामिल हैं जो अधिक सुगम और शक्तिशाली APIs प्रदान करती हैं। किसी वेब पेज को डाउनलोड करते समय HTTP रिस्पॉन्स कोड, इनपुट/आउटपुट प्रोसेसिंग, और एरर हैंडलिंग जैसे विवरण महत्वपूर्ण होते हैं। 

## संबंधित सूत्र (See Also)
- OkHttp: https://square.github.io/okhttp/
- Retrofit: https://square.github.io/retrofit/
- Kotlin I/O: https://kotlinlang.org/docs/io.html
- जेटपैक कम्पोज़: https://developer.android.com/jetpack/compose
