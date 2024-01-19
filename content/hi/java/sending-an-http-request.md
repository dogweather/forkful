---
title:                "http अनुरोध भेजना"
html_title:           "Elixir: http अनुरोध भेजना"
simple_title:         "http अनुरोध भेजना"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? क्या और क्यों?

HTTP संचारण अनुरोध, या सरलरूप से, HTTP request एक वेबसाइट को ब्राउज़र से जानकारी मांगने के लिए उपयोग होता है। यह वेब विकास में मुख्य तंत्र है। कार्यक्रमकर्ताओं को इसे उपयोग करने की आवश्यकता तब होती है जब उन्हें दूरस्थ सर्वर से डेटा पुल करने की जरूरत होती है।

## कैसे:

जावा मे HTTP request भेजने के लिए हम java.net.http package का उपयोग करेंगे। आइए एक सरल कोड के माध्यम से समझाते हैं:

```Java
import java.net.URI;
import java.net.http.HttpClient;
import java.net.http.HttpRequest;
import java.net.http.HttpResponse;

public class SendHTTP {
    public static void main(String[] args) throws Exception {
        HttpClient client = HttpClient.newHttpClient();
        HttpRequest request = HttpRequest.newBuilder()
              .uri(URI.create("http://example.com/"))
              .build();

        HttpResponse<String> response =
              client.send(request, HttpResponse.BodyHandlers.ofString());

        System.out.println(response.body());
    }
}
```

इसमें, मैंने http://example.com/ URL पर GET अनुरोध भेजा है, और फिर उसका प्रतिसाद प्रिंट किया है। 

## गहरी दुविधा

### ऐतिहासिक संदर्भ
HTTP अनुरोध भेजने का तरीका काफी समय से एक महत्वपूर्ण हिस्सा रहा है वेब विकास का। HTTP 1.0 और HTTP 1.1 में इसकी कार्यवाही काफी धीमी होती थी, लेकिन HTTP2 में यह प्रक्रिया समृद्ध हो गई।

### विकल्प
`java.net.http` package के अलावा, विकल्पी के रूप आप Apache HttpClient और OkHttp जैसी ओपन सोर्स पुस्तकालयों का उपयोग कर सकते हैं।

### क्रियानुशासन विवरण
उपरोक्त कोड क्लिप में, हमने HttpClient object का निर्माण किया, जिसके बाद हमने HttpRequest object तैयार की। इसके बाद, हमने अनुरोध को `.send()` method के माध्यम से भेजा, और फिर उसका प्रतिसाद प्रिंट किया।

## देखें भी:
- [Oracleने Official Java Documentation](https://docs.oracle.com/en/java/javase/13/network/java-net-http-httpclient.html)
- [Apache HttpClient पुस्तकालय](https://hc.apache.org/httpcomponents-client-4.5.x/quickstart.html)
- [OkHttp पुस्तकालय](https://square.github.io/okhttp/)