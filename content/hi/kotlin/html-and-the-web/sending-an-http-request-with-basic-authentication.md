---
date: 2024-01-20 18:02:14.373941-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: ."
lastmod: '2024-03-13T22:44:52.258444-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u092C\u0947\u0938\u093F\u0915 \u092A\u094D\u0930\u092E\u093E\u0923\u0940\u0915\
  \u0930\u0923 \u0915\u0947 \u0938\u093E\u0925 HTTP \u0905\u0928\u0941\u0930\u094B\
  \u0927 \u092D\u0947\u091C\u0928\u093E"
weight: 45
---

## कैसे करें:
```Kotlin
import java.net.Authenticator
import java.net.PasswordAuthentication
import java.net.URL
import javax.net.ssl.HttpsURLConnection

fun main() {
    val url = URL("https://your-api-endpoint.com/data")
    Authenticator.setDefault(object : Authenticator() {
        override fun getPasswordAuthentication(): PasswordAuthentication {
            return PasswordAuthentication("username", "password".toCharArray())
        }
    })

    with(url.openConnection() as HttpsURLConnection) {
        requestMethod = "GET"

        println("\nSending 'GET' request to URL : $url")
        println("Response Code : $responseCode")
        inputStream.bufferedReader().use {
            it.lines().forEach { line -> println(line) }
        }
    }
}
```
इस कोड में, हम URL और Basic authentication सेट कर रहे हैं और response को प्रिंट कर रहे हैं।

## गहराई से जानकारी:
Basic authentication एक HTTP authentication protocol है जिसमें username और password Base64 में encode करके HTTP header में भेजे जाते हैं। यह सरल और व्यापक रूप से समर्थित है, लेकिन इसकी सुरक्षा कमजोर मानी जाती है क्योंकि अगर ट्रैफ़िक को इंटरसेप्ट किया जाता है, तो क्रेडेंशल्स चोरी हो सकते हैं। इसी कारण इसे HTTPS के साथ इस्तेमाल करना चाहिए।

वैकल्पिक तौर पर, OAuth, टोकन बेस्ड authentication, और API keys जैसे ज़्यादा सुरक्षित मार्ग भी हैं। Basic authentication का इस्तेमाल अब भी निजी या न के बराबर सुरक्षा जरूरत वाले एपीआईज के लिए किया जाता है।

इम्प्लीमेंटेशन की बात करें तो, `Authenticator` का उपयोग ग्लोबल डिफ़ॉल्ट authentication को सेट करने के लिए किया जाता है। `HttpsURLConnection` का इस्तेमाल HTTP methods को निर्धारित करने और सर्वर से प्रतिक्रिया पाने के लिए किया जाता है।

## संबंधित सूत्र:
- [कोटलिन आधिकारिक डॉक्यूमेंटेशन](https://kotlinlang.org/docs/home.html)
- [RFC 7617, The 'Basic' HTTP Authentication Scheme](https://tools.ietf.org/html/rfc7617)
- [OKHTTP, एक लोकप्रिय HTTP & HTTP/2 client के लिए Kotlin/Java लाइब्रेरी](https://square.github.io/okhttp/)
- [वेब सेवाओं के लिए Spring Security](https://spring.io/guides/topicals/spring-security-architecture)
