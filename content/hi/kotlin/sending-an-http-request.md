---
title:                "Kotlin: एक http अनुरोध भेजना"
simple_title:         "एक http अनुरोध भेजना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/sending-an-http-request.md"
---

{{< edit_this_page >}}

## क्यों

एचटीटीपी अनुरोध भेजने में कोई काम नहीं है क्योंकि एचटीटीपी अनुरोध सर्वर से डेटा या संसाधनों को अपडेट करने के लिए अन्य साइटों और वेब सर्विसेज़ से मदद करता है। इसके अलावा, कई ऐप्स भी अन्य तकनीकी सीमाओं को पार करने के लिए एचटीटीपी अनुरोध के साथ काम करते हैं।

## कैसे करें

एचटीटीपी अनुरोध भेजने के लिए, सबसे पहले हमें URL विलोम यानी वह साइट का पता जिस से हम डेटा या संसाधनों को प्राप्त करना चाहते हैं और उसके बाद कुछ डेटा भेजने को में POST या GET जैसे होगा उन जानकारियों प्राप्त देखते हैं। कोटलिन में, हम एक HttpURLConnection ऑब्जेक्ट क्रिएट कर सकते हैं और उसके अंदर एचटीटीपी अनुरोध को भेज सकते हैं। नीचे दिए गए कोड ब्लॉक में हम एक GET रिक्वेस्ट को कैसे भेज सकते हैं दिखाएंगे।

```Kotlin
val url = URL("https://example.com") // URL बनाएं
val connection = url.openConnection() as HttpURLConnection // HttpURLConnection बनाएं
connection.requestMethod = "GET" // रिक्वेस्ट मेथड सेट करें
val responseCode = connection.responseCode // रिस्पॉन्स कोड प्राप्त करें
println("Response Code: $responseCode") 

// यदि रिस्पॉन्स कोड 200 है, तो डेटा प्राप्त करें
if(responseCode == 200) {
    val inputStream = connection.inputStream // इनपुट स्ट्रीम प्राप्त करें
    val bufferedReader = BufferedReader(InputStreamReader(inputStream))
    var inputLine: String?
    val response = StringBuffer()
    while(bufferedReader.readLine().also { inputLine = it } != null) {
        response.append(inputLine)
    }
    bufferedReader.close()
    println("Response Data: $response") // रिस्पॉन्स डेटा प्रिंट करें
}
```

## गहराई में जाएं

एचटीटीपी अनुरोध भेजने के पीछे के तकनीकी पक्षों से, ह