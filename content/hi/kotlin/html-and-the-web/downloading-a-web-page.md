---
date: 2024-01-20 17:45:03.190479-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902? (How to:) \u0915\u094B\
  \u091F\u0932\u093F\u0928 \u092E\u0947\u0902 \u0935\u0947\u092C \u092A\u0947\u091C\
  \ \u0921\u093E\u0909\u0928\u0932\u094B\u0921 \u0915\u0930\u0928\u0947 \u0915\u0947\
  \ \u0932\u093F\u090F `HttpURLConnection` \u0915\u094D\u0932\u093E\u0938 \u0915\u093E\
  \ \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0924\u0947 \u0939\u0948\u0902."
lastmod: '2024-04-05T21:53:54.264924-06:00'
model: gpt-4-1106-preview
summary: "(How to:) \u0915\u094B\u091F\u0932\u093F\u0928 \u092E\u0947\u0902 \u0935\
  \u0947\u092C \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\u094B\u0921 \u0915\
  \u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F `HttpURLConnection` \u0915\u094D\
  \u0932\u093E\u0938 \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0924\
  \u0947 \u0939\u0948\u0902."
title: "\u0935\u0947\u092C \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\u094B\
  \u0921 \u0915\u0930\u0928\u093E"
weight: 42
---

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
