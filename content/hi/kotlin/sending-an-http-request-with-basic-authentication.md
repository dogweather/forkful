---
title:                "बेसिक प्रमाणीकरण के साथ एक एचटीटीपी अनुरोध भेजना"
html_title:           "Kotlin: बेसिक प्रमाणीकरण के साथ एक एचटीटीपी अनुरोध भेजना"
simple_title:         "बेसिक प्रमाणीकरण के साथ एक एचटीटीपी अनुरोध भेजना"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्यों

एक उपयोगकर्ता को अपने ऐप को एक्सटर्नल स्रोत से डेटा फेच करने की आवश्यकता हो सकती है, जो एक सिस्टम से डेटा को हासिल करने के लिए HTTP अनुरोधों का उपयोग करता है। इस स्थिति में, उपयोगकर्ता बेसिक ऑथेंटिकेशन के साथ HTTP अनुरोध भेजता है जो उन्हें किसी भी सुरक्षित अनुरोध को संसाधित करने की अनुमति देता है।

## कैसे करें

आइए सीखें कि आप बेसिक ऑथेंटिकेशन के साथ HTTP अनुरोध कैसे भेज सकते हैं और अपने ऐप से डेटा को कैसे प्राप्त कर सकते हैं। सबसे पहले, आपको अपने ऐप को आपोजिंग कॉनेक्शन टाइमआउट से जोड़ना होगा। उसके बाद, निम्नलिखित कोटलिन कोड ब्लॉक में आपको अपने ऐप के दूसरे समूह में डेटा को हासिल करने के लिए एक HTTP क्लाइंट को बनाना होगा:

```Kotlin
val url = URL("यूआरएल_ऑफ_डाटा")
val httpURLConnection = url.openConnection() as HttpStRLConnection
httpURLConnection.setRequestMethod("GET")
httpURLConnection.setRequestProperty("Authorization", "Basic " + Base64.getEncoder().encodeToString("यूजरनेम:पासवर्ड".toByteArray()))

val responseCode = httpURLConnection.responseCode
if (responseCode == HttpsURLConnection.HTTP_OK) {
    val inputStream = BufferedInputStream(httpURLConnection.inputStream)
    val bufferedReader = BufferedReader(InputStreamReader(inputStream))
    val stringBuilder = StringBuilder()
    var inputLine: String? = bufferedReader.readLine()
    while (inputLine != null) {
        stringBuilder.append(inputLine)
        inputLine = bufferedReader.readLine()
    }
    bufferedReader.close()
    val response = stringBuilder.toString()
    // यहां आप अपने रिस्पॉन्स को उपयोग कर सकते हैं
} else {
    // अगर ऑथेंटिकेशन में कोई गड़बड़ी होती है तो यहां आपको थ्रो एक एक्सेप्शन करना होगा
}
```

तुमने क्या देखा? आपने अपने ऐप से एक HTTP अनुरोध भेजा है। आपने इसके लिए