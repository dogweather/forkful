---
title:                "Elixir: बेसिक प्रमाणीकरण के साथ HTTP अनुरोध भेजें"
simple_title:         "बेसिक प्रमाणीकरण के साथ HTTP अनुरोध भेजें"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

Hindi: हिंदी में Elixir प्रोग्रामिंग ब्लॉग पोस्ट का उपयोग करके लिखें।
# क्यों

एक HTTP अनुरोध को बेसिक प्रमाणीकरण के साथ भेजने में क्यों लिया जाता है का अवलोकन करने के लिए।

## कैसे करें

आप अपने Elixir एप्लिकेशन में HTTP अनुरोध को बेसिक प्रमाणीकरण के साथ कैसे भेज सकते हैं, इसके लिए निम्न उदाहरणों का उपयोग कर सकते हैं।

```elixir
defmodule BasicAuthExample do
  def send_request do
    username = "john"
    password = "pass123"
    auth_header = "Basic #{Base.encode64("#{username}:#{password}")}"

    HTTPoison.get("https://example.com/api", [], [headers: [{"Authorization", auth_header}]])
  end
end

BasicAuthExample.send_request()
```

उपरोक्त कोड ब्लॉक में, हमने "john" और "pass123" उपयोग करके अपना उपयोगकर्ता नाम और पासवर्ड सेट किए हैं और फिर उनको Base 64 इन्कोड करके बेसिक प्रमाणीकरण हैडर में शामिल किया है। HTTPoison.get() फंक्शन को "Authorization" हैडर के साथ बुलाया गया है जो हमारे कोड को उपयोगकर्ता के प्रमाणीकरण जानकारी के साथ निर्धारित URL पर अनुरोध भेजने में मदद करता है। उपर्युक्त उदाहरण को अपनी आवश्यकताओं के अनुसार संशोधित कर सकते हैं और अपने HTTP अनुरोध को सफलतापूर्वक बेसिक प्रमाणीकरण के साथ भेज सकते हैं।

## गहराई में विवेचना

HTTP अनुवेदकों के माध्यम से अनुरोध के स्थान पर, हम बेसिक प्रमाणीकरण कोड द्वारा प्रमाणीकरण हैडर में उपयोगकर्ता नाम और पासवर्ड को शामिल करके सुरक्षित रूप से HTTP अनुरोध भेज सकते हैं। यदि उदाहर