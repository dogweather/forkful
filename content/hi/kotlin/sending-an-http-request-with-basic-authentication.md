---
title:                "बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
html_title:           "C#: बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
simple_title:         "बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

# एचटीटीपी अनुरोध के साथ बुनियादी प्रमाणीकरण का उपयोग करना एक Kotlin application में

## क्या और क्यों?

HTTP अनुरोध के साथ Basic Authentication का उपयोग सुरक्षा का एक स्तर प्रदान करता है, जिससे केवल विशिष्ट उपयोगकर्ताओं को ही विशेष संसाधनों तक पहुंच प्राप्त होती है। यह प्रमाणिकरण की रूपरेखा निर्धारित करता है जिसे वेब सर्वर प्रमाणित करेंगे।

## कैसे करें:

आइए देखें कैसे हम एक HTTP अनुरोध को Kotlin में बुनियादी प्रमाणीकरण के साथ भेजते हैं।

```kotlin
import java.net.URL
import java.net.HttpURLConnection
import java.util.Base64

fun main() {
    // सर्वर का URL
    val url = URL("http://your.server.com")

    // HTTP connection खोलें
    val connection = url.openConnection() as HttpURLConnection

    // यूज़रनेम और पासवर्ड
    val userCredentials = "username:password"

    // Basic Authentication Header में encode करें
    val basicAuth = "Basic " + String(Base64.getEncoder().encode(userCredentials.toByteArray()))

    // Header को set करें
    connection.setRequestProperty ("Authorization", basicAuth)

    // कनेक्शन का प्रयोग करें
    connection.inputStream.bufferedReader().use {
        println(it.lines().collect(Collectors.joining("\n")))
    }
}
```

इस कोड का उद्धरण दे रहा है की कैसे एक Basic Authentication Header को set किया जाता है और URL को call कैसे किया जाता है।


## गहरी डाइव:

HTTP Basic Authentication का एक सरल लेकिन उपयोगी उपकरण है जिसे 1990s द्वारा विकसित किया गया था। यह तभी कार्य करता है जब एक वेब सर्वर और एक ग्राहक (मामले में, हमारे कोड) के बीच SSL / TLS एन्क्रिप्टेड कनेक्शन हो। यदि आवश्यक हो तो Digest Access Authentication जैसे अन्य विकल्प भी हैं जो Basic Authentication की कमजोरियों को दूर कर सकते हैं। हालांकि, बेहतर सुरक्षा की आवश्यकता होने पर, आपको OAuth जैसी अधिक सुरक्षित प्रमाणीकरण रणनीतियों का अन्वेषण करना चाहिए। कठिनाई इस बात की हो सकती है कि आपको प्रत्येक सर्वर के लिए Header को Customise करना चाहिए।

## यह भी देखें:

1. [Mozilla's Web Docs: HTTP Authentication](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
2. [Oracle's Java HttpUrlConnection Docs](https://docs.oracle.com/javase/8/docs/api/java/net/HttpURLConnection.html)
3. [Baeldung's Guide to Basic Authentication with HttpClient 4](https://www.baeldung.com/httpclient-basic-authentication)