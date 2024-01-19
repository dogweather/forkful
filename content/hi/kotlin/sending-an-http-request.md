---
title:                "http अनुरोध भेजना"
html_title:           "Elixir: http अनुरोध भेजना"
simple_title:         "http अनुरोध भेजना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/sending-an-http-request.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
HTTP अनुरोध भेजना का मतलब है कि आप किसी नेटवर्क सर्वर से जानकारी मांग रहे हैं या उसे भेज रहे हैं। कार्यक्रम के लिए यह आवश्यक है क्योंकि यह वेब पेजेस, डेटाबेस, और अन्य नेटवर्क संसाधनों से डाटा प्राप्त करने की क्षमता प्रदान करता है।

## कैसे करें:
Kotlin में HTTP अनुरोध भेजने के लिए, कोिटलिन के Ktor क्लाइंट का उपयोग करें। निम्नलिखित कोड देखें:

```Kotlin
import io.ktor.client.*
import io.ktor.client.request.*

suspend fun main() {
   val client = HttpClient()
   val content: String = client.get("https://ktor.io/")
   print(content)
   client.close()
}
```
ऊपर दिए गए कोड का निष्पादन करते समय, आपको "https://ktor.io/" URL की सामग्री प्रिंट होगी।

## गहरे डाइव
HTTP अनुरोध वेब संचार का मुख्य तत्व है और 1990 में टिम बर्नर्स-ली द्वारा विकसित किया गया था। कोई भी मशीन, जैसे कि ब्राउज़र, या आपका कोटलिन कोड, जो HTTP प्रोटोकॉल का उपयोग कर सकती है, वे अन्य मशिनों (जैसे कि वेबसाइट) से जानकारी मांग सकती हैं। 

वैकल्पिक रूप से, आप कंडलिट में ऑक्टोकिट, फ्यूल, आदि का भी उपयोग कर सकते हैं। 

हर अनुरोध के बाद, आपको यह देखना चाहिए कि अनुरोध ने उचित उत्तर दिया है या नहीं। जांचने के लिए आप `HttpResponseValidator` का उपयोग कर सकते हैं।

## यह भी देखें
- Kotlin Ktor क्लाएंट डॉक्युमेंटेशन [https://ktor.io/docs/http-client.html](https://ktor.io/docs/http-client.html)
- HTTP रिक्वेस्ट बनाने और प्रबंधित करने के लिए Kotlin के मार्जरेास्ट लाइब्रेरी [https://github.com/ktorio/ktor](https://github.com/ktorio/ktor)
- HTTP प्रोटोकॉल के बारे में अधिक जानकारी [https://developer.mozilla.org/en-US/docs/Web/HTTP](https://developer.mozilla.org/en-US/docs/Web/HTTP)