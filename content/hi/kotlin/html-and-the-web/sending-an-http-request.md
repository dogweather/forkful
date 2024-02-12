---
title:                "HTTP अनुरोध भेजना"
aliases: - /hi/kotlin/sending-an-http-request.md
date:                  2024-01-20T18:00:33.756561-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTP अनुरोध भेजना"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/sending-an-http-request.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
HTTP अनुरोध भेजने का मतलब है वेब सर्वर से जानकारी मांगना या उसे डेटा भेजना। प्रोग्रामर्स ऐसा तब करते हैं जब उनके एप्लिकेशन को इंटरनेट पर डेटा एक्सेस करना होता है या शेयर करना होता है।

## कैसे करें:
```Kotlin
import java.net.HttpURLConnection
import java.net.URL

fun main() {
    val url = URL("http://example.com") // अपना URL डालें
    with(url.openConnection() as HttpURLConnection) {
        requestMethod = "GET"  // या "POST", "PUT", "DELETE", आदि
        
        println("Response Code: $responseCode")
        inputStream.bufferedReader().use {
            it.lines().forEach { line ->
                println(line)
            }
        }
    }
}
```
सैंपल आउटपुट:
```
Response Code: 200
<!doctype html>
<html>
<head>
...
</head>
<body>
...
</body>
</html>
```

## गहराई में:
HTTP अनुरोध भेजना इंटरनेट के माध्यम से संवाद करने का एक मौलिक तरीका है जिसे 1990 के दशक से प्रयोग किया जा रहा है। जहां Java में `HttpURLConnection` उपयोग किया जाता था, वहीं Kotlin में हम इसे अधिक सरलता से कर सकते हैं और बेहतर सिंटैक्स के साथ। कुछ विकल्पों में लाइब्रेरीज जैसे कि OkHttp, Retrofit, या Ktor होती हैं, जो और अधिक सुविधाजनक एपीआई प्रदान करती हैं और जटिलताओं को सरल बनाती हैं। इन्हें उपयोग करते हुए आप प्राप्ती, परिचीती, और प्रबंधन को बेहतर तरीके से संचालित कर सकते हैं।

## इसे भी देखें:
- [OkHttp GitHub Repository](https://github.com/square/okhttp)
- [Kotlin के लिए Ktor](https://ktor.io/)
- [Retrofit GitHub Repository](https://github.com/square/retrofit)

नोट: जिन भारतीय पाठकों के लिए यह आर्टिकल लिखा गया है, वे ऊपर दिए गए लिंक्स पर जाकर कोडिंग में इस्तेमाल होने वाली लाइब्रेरीज के बारे में और जान सकते हैं।
