---
date: 2024-01-20 18:02:07.504252-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902? (How to:) ."
lastmod: '2024-03-13T22:44:52.113109-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u092C\u0947\u0938\u093F\u0915 \u092A\u094D\u0930\u092E\u093E\u0923\u0940\u0915\
  \u0930\u0923 \u0915\u0947 \u0938\u093E\u0925 HTTP \u0905\u0928\u0941\u0930\u094B\
  \u0927 \u092D\u0947\u091C\u0928\u093E"
weight: 45
---

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
