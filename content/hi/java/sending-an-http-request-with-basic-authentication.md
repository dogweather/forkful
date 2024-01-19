---
title:                "बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
html_title:           "C#: बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
simple_title:         "बेसिक प्रमाणीकरण के साथ http अनुरोध भेजना"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
HTTP अनुरोध वितारण जिसमे बेसिक प्रमाणन (basic authentication) होता है एक मूलभूत क्रिया है । इसका प्रयोग वेब सर्वरसे डेटा लेने या डेटा गुजारने के लिए किया जाता है। यह सुरक्षित तरीका होता है APIs से जुड़ने का।

## कैसे करें:
```Java
import java.net.http.HttpClient;
import java.net.http.HttpHeaders;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;
import java.nio.charset.StandardCharsets;
import java.util.Base64;

public class HttpBasicAuth {

    public static void sendRequest() throws Exception {

        HttpClient client = HttpClient.newHttpClient();

        String auth = "user:password";
        String encodedAuth = Base64.getEncoder()
          .encodeToString(auth.getBytes(StandardCharsets.UTF_8));

        HttpRequest request = HttpRequest.newBuilder()
          .uri(new URI("http://your-url.com"))
          .header("Authorization", "Basic " + encodedAuth)
          .build();

        HttpResponse<String> response = client.send(request,
          HttpResponse.BodyHandlers.ofString());

        System.out.println(response.statusCode());
        System.out.println(response.body());
    }

    public static void main(String[] args) throws Exception {
        sendRequest();
    }
}
```
उपरोक्त कोड सारी आवश्यकताओं को पूरा करेगा और सर्वर से प्रतिक्रिया आने पर इसे छाप देगा।

## गहराई में:
HTTP और Basic Authentication की जोड़ी का इतिहास वेब के आरम्भिक दिनों से ही जुड़ी हुई है। बेसिक प्रमाणन (Basic Authentication) के विकल्प OAuth और Digest Access Authentication होते हैं। हालांकि, बेसिक प्रमाणन सबसे आसान होता है लेकिन इसे HTTPS के साथ उपयोग करना चाहिए क्योंकि यह क्रेडेंशियल्स को एन्कोड करता है, न कि एन्क्रिप्ट। 

## See Also:
1. जावा नेटवर्किंग (भाग 1) - Socket, ServerSocket, और InetAdress ट्यूटोरियल: https://www.guru99.com/java-networking.html 
2. जावा के साथ HTTP सर्वर तक पहुंचना: https://www.baeldung.com/java-http-request 
3. ऑथोराइज़ेशन हेडर्स और एचटीटीपी अनुरोध: https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Authorization
4. Basic और Digest Access Authentication का RFC: https://tools.ietf.org/html/rfc2617