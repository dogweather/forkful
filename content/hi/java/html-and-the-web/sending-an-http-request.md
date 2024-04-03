---
date: 2024-01-20 18:00:40.413448-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902?) Java \u092E\
  \u0947\u0902 HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u0928\u0947\
  \ \u0915\u0947 \u0932\u093F\u090F `java.net.http` \u092A\u0948\u0915\u0947\u091C\
  \ \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0939\u094B\u0924\u093E \u0939\
  \u0948\u0964 \u092F\u0947 \u0906\u092A\u0915\u094B \u0906\u0938\u093E\u0928\u0940\
  \ \u0938\u0947 GET \u0914\u0930 POST \u0905\u0928\u0941\u0930\u094B\u0927 \u0915\
  \u0930\u0928\u0947 \u0926\u0947\u0924\u093E \u0939\u0948\u0964 \u092F\u0939\u093E\
  \u0901 \u090F\u0915\u2026"
lastmod: '2024-03-13T22:44:52.108270-06:00'
model: gpt-4-1106-preview
summary: "Java \u092E\u0947\u0902 HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\
  \u0947\u091C\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F `java.net.http` \u092A\u0948\
  \u0915\u0947\u091C \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0939\u094B\
  \u0924\u093E \u0939\u0948\u0964 \u092F\u0947 \u0906\u092A\u0915\u094B \u0906\u0938\
  \u093E\u0928\u0940 \u0938\u0947 GET \u0914\u0930 POST \u0905\u0928\u0941\u0930\u094B\
  \u0927 \u0915\u0930\u0928\u0947 \u0926\u0947\u0924\u093E \u0939\u0948\u0964 \u092F\
  \u0939\u093E\u0901 \u090F\u0915 GET \u0905\u0928\u0941\u0930\u094B\u0927 \u0915\u093E\
  \ \u0909\u0926\u093E\u0939\u0930\u0923 \u0939\u0948."
title: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u0928\u093E"
weight: 44
---

## How to: (कैसे करें?)
Java में HTTP अनुरोध भेजने के लिए `java.net.http` पैकेज इस्तेमाल होता है। ये आपको आसानी से GET और POST अनुरोध करने देता है। यहाँ एक GET अनुरोध का उदाहरण है:

```java
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.io.IOException;
import java.net.URISyntaxException;

public class HttpExample {
    public static void main(String[] args) throws IOException, InterruptedException, URISyntaxException {
        HttpClient client = HttpClient.newHttpClient();
        HttpRequest request = HttpRequest.newBuilder()
                              .uri(new URI("http://httpbin.org/get"))
                              .build();
        HttpResponse<String> response = client.send(request, HttpResponse.BodyHandlers.ofString());
        System.out.println(response.body());
    }
}
```

यदि आप उपरोक्त कोड को चलाएंगे, तो आपको जवाब में JSON डेटा मिलेगा, जो `http://httpbin.org/get` सर्वर से आपके GET अनुरोध का डिटेल्स होगा।

## Deep Dive (गहराई से जानकारी)
जावा में HTTP अनुरोध की सुविधा पहले `HttpURLConnection` क्लास से मिलती थी, लेकिन Java 11 से `java.net.http.HttpClient` यह ज्यादा सहज और मॉडर्न विकल्प बन गया है। हम `HttpRequest.Builder` से विभिन्न प्रकार के HTTP अनुरोध तैयार कर सकते हैं, जैसे GET, POST, PUT, DELETE आदि। `HttpClient` असिंक्रोनस अनुरोध भेजने की सुविधा भी देता है, जिससे ऐप्लिकेशन के प्रदर्शन में सुधार होता है।

कई बार हमें तीसरे पक्ष के libraries जैसे कि Apache HttpClient, OkHttp आदि का इस्तेमाल करना पड़ता था, पर Java 11 के बाद से Java की Core API में ही यह सुविधा आ गई है। Implementation में `HttpClient` इंटरनल रूप से NIO (Non-blocking I/O) का इस्तेमाल करता है जो अनुरोधों को ज्यादा कुशलता से संभालता है।

## See Also (और देखें)
और जानने के लिए, निम्नलिखित रिसोर्सेज मददगार हो सकते हैं:

- [Java™ SE Development Kit 11 Documentation](https://docs.oracle.com/en/java/javase/11/docs/api/java.net.http/java/net/http/HttpClient.html)
- [HTTP अनुरोधों के लिए Official Java Tutorial](https://docs.oracle.com/javase/tutorial/networking/urls/readingWriting.html)
- [httpbin.org](http://httpbin.org/) - HTTP अनुरोध परीक्षा करने के लिए वेबसाइट
- [Mozila MDN Web Docs: HTTP](https://developer.mozilla.org/en-US/docs/Web/HTTP) - HTTP के बारे में गहराई से समझने के लिए
