---
title:                "एक एचटीटीपी अनुरोध भेजना"
html_title:           "Kotlin: एक एचटीटीपी अनुरोध भेजना"
simple_title:         "एक एचटीटीपी अनुरोध भेजना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/sending-an-http-request.md"
---

{{< edit_this_page >}}

## क्यों

HTTP (Hypertext Transfer Protocol) एक प्रोटोकॉल है जिसका उपयोग वेब डेवलपमेंट में किया जाता है और यह अन्य कंप्यूटर मशीनों में डेटा को भेजने और प्राप्त करने के लिए इस्तेमाल होता है। HTTP रिक्वेस्ट भेजना सबसे साधारण तरीका है जो आपको दूसरे वेब सर्वरों के साथ संवाद करने में मदद करता है। कंप्यूटर सॉफ्टवेयर डेवलपमेंट में, आप HTTP रिक्वेस्ट द्वारा डेटा को भेजकर काम को आसान बना सकते हैं।

## कैसे करें

```Kotlin
fun main() {
    val url = "https://google.com"
    val client = OkHttpClient()

    val request = Request.Builder()
        .url(url)
        .build()

    val response = client.newCall(request).execute()
    println(response.body?.string())
}
```

इस कोड के अंतर्गत, हमने हुमारी प्रथम HTTP रिक्वेस्ट भेजी है। पहले एक यूआरएल बनाते हैं जो हमें कंप्यूटर सभ्दारण में भेजना है। फिर हमने OkHttpClient क्लास का एक नया ऑब्जेक्ट बनाया है जो हमारी रिक्वेस्ट को भेजने में मदद करता है। हमने एक Request बिल्डर भी बनाया है जिसमें हमने हमारे यूआरएल को सेट किया है और फिर यह बिल्ड किया गया request ऑब्जेक्ट है। अंत में, हमने client को कहा है कि response को execute करें और फिर हमने उसके बॉडी को प्रिंट किया है। यदि सब कुछ सही से काम करता है, तो आपको google.com का HTML देखना चाहिए।

## गहराई में

HTTP रिक्वेस्ट हमें कनेक्शन को स्थापित करने और उससे डेटा को भेजने के लिए एक स्टेप ऑवर है। यदि आपको अधिक जानकारी चाहिए तो [okhttp डॉक्यूमें