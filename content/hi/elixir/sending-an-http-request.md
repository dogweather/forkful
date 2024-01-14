---
title:                "Elixir: HTTP अनुरोध भेजना"
simple_title:         "HTTP अनुरोध भेजना"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

### क्यों

एक HTTP अनुरोध भेजने का सही तरीका सिर्फ आपको प्रोग्रामिंग के लिए एक महत्वपूर्ण यूज़ केस है, वरना आप एप्लिकेशन के साथ संघर्ष कर सकते हैं।

### कैसे करें

``` Elixir
# कनेक्शन निर्माण करें
conn = HTTPoison.get("https://example.com")

# आउटपुट की जांच करें
IO.puts(conn)
```

आउटपुट:

``` Elixir
%HTTPoison.Response{status_code: 200, body: "वेबसाइट का सामग्री", headers: [...]}
```

### गहराई में जाएं

HTTP अनुरोध भेजने के लिए, हम प्रस्तावित URL पर एक आरूपक्रिया का निर्माण करते हैं। यह अनुरोध जारी रखता है, प्राप्त उत्तर को प्रसंस्करण करता है और प्रतिक्रिया को हमारे कोड में जमा करता है। जोड़ने के लिए, हम HTTPoison लाइब्रेरी का उपयोग कर सकते हैं जो हमेशा उपलब्ध होती है।

### देखें भी

- [Elixir के साथ HTTP कैसे भेजें]()
- [Elixir में HTTP प्रोटोकॉल की समझ]()
- [Elixir डॉक्स HTTPoison]()