---
title:                "बेसिक प्रमाणीकरण के साथ HTTP अनुरोध भेजना"
aliases:
- hi/java/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:02:07.504252-07:00
model:                 gpt-4-1106-preview
simple_title:         "बेसिक प्रमाणीकरण के साथ HTTP अनुरोध भेजना"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
HTTP रिक्वेस्ट बेसिक ऑथेंटिकेशन के साथ भेजना एक प्रक्रिया है जहां सर्वर को यूजरनेम और पासवर्ड कोडित फॉर्म में भेजा जाता है। प्रोग्रामर्स इसे डाटा एक्सेस करने या सेंसिटिव एक्शन्स को ऑथराइज़ करने के लिए करते हैं।

## कैसे करें? (How to:)
```java
import java.io.IOException;
import java.net.HttpURLConnection;
import java.net.URL;
import java.util.Base64;

public class BasicAuthRequest {

    public static void main(String[] args) throws IOException {
        URL url = new URL("http://your-api-endpoint.com/data");
        String credentials = "username:password";
        String basicAuthPayload = "Basic " + Base64.getEncoder().encodeToString(credentials.getBytes());

        HttpURLConnection connection = (HttpURLConnection) url.openConnection();
        connection.setRequestMethod("GET");
        connection.setRequestProperty("Authorization", basicAuthPayload);

        int responseCode = connection.getResponseCode();
        System.out.println("Response Code : " + responseCode);
        // Output will depend on your API's response. E.g., "Response Code : 200" indicates success.
    }
}
```

## गहन अध्ययन (Deep Dive)
HTTP बेसिक ऑथेंटिकेशन सबसे सरल ऑथेंटिकेशन तकनीकों में से एक है और इसकी शुरुआत 1990 के दशक में हुई थी। यह एक तटस्थ तकनीक है, जो HTTPS के उपयोग के बिना असुरक्षित हो सकती है, क्योंकि क्रेडेंशियल्स बेस64 कोडेड फॉर्म में होते हैं, जिन्हें आसानी से डिकोड किया जा सकता है। अधिक सुरक्षित विकल्पों में OAuth और API keys शामिल हैं। जहां तक विवरण की बात है, Java नेटवर्किंग के माध्यम से HttpURLConnection क्लास का उपयोग करते हुए बेसिक ऑथेंटिकेशन हेडर सेट करता है।

## सम्बंधित स्रोत (See Also)
- [HTTP बेसिक ऑथेंटिकेशन (MDN)](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [Java HttpURLConnection डॉक्यूमेंटेशन](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/net/HttpURLConnection.html)
- [Base64 Encoding और Decoding इन Java](https://docs.oracle.com/en/java/javase/17/docs/api/java.base/java/util/Base64.html)
