---
title:                "Kotlin: बेसिक प्रमाणीकरण के साथ एक http अनुरोध भेजना।"
simple_title:         "बेसिक प्रमाणीकरण के साथ एक http अनुरोध भेजना।"
programming_language: "Kotlin"
category:             "Kotlin"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/kotlin/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्यों

HTTP रिक्वेस्ट को भेजने के लिए बेसिक ऑथेंटिकेशन का उपयोग करने का कारण है कि यह सुरक्षित और सत्यापित तरीके से सर्वर से डेटा अनुरोध करने का सरल और प्रभावी तरीका है। 

## कैसे करें

```Kotlin
val url = "http://www.example.com/api/resource"
var connection = URL(url).openConnection() as HttpURLConnection
connection.setRequestProperty("Authorization", "Basic <यूजरनेम>:<पासवर्ड>".toByteArray().encodeBase64())
connection.requestMethod = "GET"

val responseCode = connection.responseCode

if(responseCode == HttpURLConnection.HTTP_OK) {
    val inputStream = connection.inputStream
    val bufferedReader = BufferedReader(InputStreamReader(inputStream))

    var input: String? = null
    val response = StringBuilder()

    while({input = bufferedReader.readLine();input}() != null) {
        response.append(input)
    }
    bufferedReader.close()
    println(response.toString())
} else {
    println("Error in response. Error code is: $responseCode")
}
```

एपीआई से डेटा अनुरोध करने के लिए सबसे पहले हम एक `URL` बनाते हैं। फिर, हम उस URL पर `openConnection()` कॉल कर एक `HttpURLConnection` ऑब्जेक्ट प्राप्त करते हैं। यहां हम `setRequestProperty()` को उदाहरण के लिए `Authorization` शीर्षक के लिए `Basic <यूजरनेम>:<पासवर्ड>` मूल्य और इसके बाद के अनमाने उपयोगकर्ता नाम और पासवर्ड से उपयोगकर्ता को पासवर्ड डेटा को कोड करने के लिए अनुरोध भेजते हैं। फिर हम `checkResponseCode()` कॉल करते हैं ताकि हम यदि सफलतापूर्वक उत्तर प्राप्त करते हैं, तो हम उस उत्तर को प्रिंट कर सकें। लम्बाई जोड़ने के लिए हम `BufferedReader` का उपयोग करते हैं जो डेटा को पढ़ने में मदद करता है। `BufferedReader` से हम उत्तर को पाठ के रूप में पढ़ते हैं और उसे `StringBuilder` में जोड़ते हैं। अंत में हम `connection` को बंद करते हैं।

यदि कोई त्रुटि होती है तो हम `print` को कॉल करते हैं और एरर कोड को प्रिंट करते ह