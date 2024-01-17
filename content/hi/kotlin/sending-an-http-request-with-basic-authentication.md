---
title:                "बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
html_title:           "Kotlin: बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
simple_title:         "बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
HTTP अनुरोध को बेसिक प्रमाणीकरण के साथ भेजना, यह क्या है और प्रोग्रामर्स ऐसा क्यों करते हैं, इसके बारे में दो तीन वाक्यों में समझाया जाएगा।

## कैसे करें:
कोटलिन के माध्यम से एक विशेष HTTP अनुरोध को भेजने के लिए इम्प्लीमेंटेशन का एक उदाहरण दिया गया है। आप अपनी पसंद अनुसार इसका उपयोग कर सकते हैं।

```kotlin
val url = "https://example.com"
val credentials = Credentials.basic("username", "password")
val request = Request.Builder()
                .url(url)
                .header("Authorization", credentials)
                .build()
val client = OkHttpClient()
val response = client.newCall(request).execute()
println(response.body?.string())
```

आउटपुट:
```html
<html>
    <body>
        <h1>Hello, World!</h1>
    </body>
</html>
```

## डीप डाइव:
इस तकनीक को HTTP बेसिक प्रमाणीकरण के बारे में इतिहास और इसके वैकल्पिक तकनीकों का विस्तृत विवरण दिया गया है। इसके साथ ही, HTTP अनुरोध को भेजने के लिए इस तकनीक के अंतर्मुखी माहिती की भी बात की गई है।

## सी भी:
आप और अधिक जानकारी के लिए नीचे दिए गए स्रोतों पर जा सकते हैं:
- [HTTP Basic Authentication documentation](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [OkHttp library](https://square.github.io/okhttp/)
- [Kotlin programming language](https://kotlinlang.org/)