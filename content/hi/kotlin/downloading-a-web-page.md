---
title:                "वेब पेज डाउनलोड करना"
date:                  2024-01-20T17:45:03.190479-07:00
model:                 gpt-4-1106-preview
simple_title:         "वेब पेज डाउनलोड करना"

category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/downloading-a-web-page.md"
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
