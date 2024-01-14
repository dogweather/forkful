---
title:                "Java: Hindi: एक HTTP अनुरोध भेजना"
simple_title:         "Hindi: एक HTTP अनुरोध भेजना"
programming_language: "Java"
category:             "Java"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/java/sending-an-http-request.md"
---

{{< edit_this_page >}}

इस लेख में हम जानेंगे कि एचटीटीपी अनुरोध भेजने के पीछे क्या कारण हो सकते हैं और इसको कैसे किया जाता है। हम जावा में इसका उदाहरण देखेंगे और उस से संबंधित गहराई में भी जानकारी प्राप्त करेंगे।

## क्यों

एचटीटीपी अनुरोध भेजना आमतौर पर किसी अन्य सर्वर से डेटा प्राप्त करने का एक सबसे आसान और तेज तरीका है। यह वेब एप्लिकेशन और स्क्रिप्ट्स में अनुरोध भेजने के लिए उपयोगी होता है, जो उन्हें संचालित करने के लिए एक सर्वर से दूसरे सर्वर को डेटा स्वीकार करने की अनुमति देता है।

## कैसे करें

एचटीटीपी अनुरोध भेजने के लिए हम `HttpURLConnection` कक्षा का उपयोग करते हैं। निम्नलिखित उदाहरण में हम इसका उपयोग करके अपने सर्वर से डेटा प्राप्त करेंगे:

```java
try {
    // सर्वर का URL बनाएं
    URL url = new URL("https://www.example.com/");
    // एचटीटीपी कनेक्शन बनाएं
    HttpURLConnection connection = (HttpURLConnection) url.openConnection();
    // अनुरोध का तरीका सेट करें
    connection.setRequestMethod("GET");
    // अनुरोध को स्वीकार करने के लिए असंयोजित स्ट्रीम प्राप्त करें
    InputStream inputStream = connection.getInputStream();
    // स्ट्रीम को पढ़ें
    InputStreamReader reader = new InputStreamReader(inputStream);
    // पढ़ें और प्रिंट करें
    int data = reader.read();
    while (data != -1) {
        System.out.print((char) data);
        data = reader.read();
    }
    // समाप्त करें
    reader.close();
} catch (IOException e) {
    e.printStackTrace();
}
```

उपरोक्त कोड को कंपाइल करने पर आपको आपके सर्वर से वेबसाइट के साथ खुलते हुए एक HTML पेज का डेटा प्राप्त होगा। आप अनुरोध का तरीका ब