---
date: 2024-01-20 18:02:07.504252-07:00
description: "HTTP \u0930\u093F\u0915\u094D\u0935\u0947\u0938\u094D\u091F \u092C\u0947\
  \u0938\u093F\u0915 \u0911\u0925\u0947\u0902\u091F\u093F\u0915\u0947\u0936\u0928\
  \ \u0915\u0947 \u0938\u093E\u0925 \u092D\u0947\u091C\u0928\u093E \u090F\u0915 \u092A\
  \u094D\u0930\u0915\u094D\u0930\u093F\u092F\u093E \u0939\u0948 \u091C\u0939\u093E\
  \u0902 \u0938\u0930\u094D\u0935\u0930 \u0915\u094B \u092F\u0942\u091C\u0930\u0928\
  \u0947\u092E \u0914\u0930 \u092A\u093E\u0938\u0935\u0930\u094D\u0921 \u0915\u094B\
  \u0921\u093F\u0924 \u092B\u0949\u0930\u094D\u092E \u092E\u0947\u0902 \u092D\u0947\
  \u091C\u093E \u091C\u093E\u0924\u093E \u0939\u0948\u0964 \u092A\u094D\u0930\u094B\
  \u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0907\u0938\u0947 \u0921\u093E\
  \u091F\u093E \u090F\u0915\u094D\u0938\u0947\u0938\u2026"
lastmod: '2024-02-25T18:49:49.315086-07:00'
model: gpt-4-1106-preview
summary: "HTTP \u0930\u093F\u0915\u094D\u0935\u0947\u0938\u094D\u091F \u092C\u0947\
  \u0938\u093F\u0915 \u0911\u0925\u0947\u0902\u091F\u093F\u0915\u0947\u0936\u0928\
  \ \u0915\u0947 \u0938\u093E\u0925 \u092D\u0947\u091C\u0928\u093E \u090F\u0915 \u092A\
  \u094D\u0930\u0915\u094D\u0930\u093F\u092F\u093E \u0939\u0948 \u091C\u0939\u093E\
  \u0902 \u0938\u0930\u094D\u0935\u0930 \u0915\u094B \u092F\u0942\u091C\u0930\u0928\
  \u0947\u092E \u0914\u0930 \u092A\u093E\u0938\u0935\u0930\u094D\u0921 \u0915\u094B\
  \u0921\u093F\u0924 \u092B\u0949\u0930\u094D\u092E \u092E\u0947\u0902 \u092D\u0947\
  \u091C\u093E \u091C\u093E\u0924\u093E \u0939\u0948\u0964 \u092A\u094D\u0930\u094B\
  \u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0907\u0938\u0947 \u0921\u093E\
  \u091F\u093E \u090F\u0915\u094D\u0938\u0947\u0938\u2026"
title: "\u092C\u0947\u0938\u093F\u0915 \u092A\u094D\u0930\u092E\u093E\u0923\u0940\u0915\
  \u0930\u0923 \u0915\u0947 \u0938\u093E\u0925 HTTP \u0905\u0928\u0941\u0930\u094B\
  \u0927 \u092D\u0947\u091C\u0928\u093E"
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
