---
title:                "HTTP अनुरोध भेजना"
html_title:           "Elixir: HTTP अनुरोध भेजना"
simple_title:         "HTTP अनुरोध भेजना"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/sending-an-http-request.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
HTTP अनुरोध भेजना एक प्रोग्रामिंग कार्य है जहां प्रोग्रामर अन्य सर्वर को डेटा को भेजने के लिए अनुरोध भेजते हैं। यह इसलिए क्योंकि डेटा साझा करने के लिए इंटरनेट का उपयोग करने का सबसे प्रमुख और आसान तरीका है।

## कैसे करें:
```elixir
# HTTP इनइक्सन के सहायता से HTTP अनुरोध भेजें
HTTPoison.get!( "https://www.example.com/" )

# HTTP कोड स्नीपेट के शुरू में एक HTTPoison इनस्टेंस पेश करके HTTP अनुरोध भेजें
defmodule MyModule do
    use HTTPoison

    HTTPoison.get!( "https://www.example.com/" )
end

# उत्पादन:
%HTTPoison.Response{ status_code: 200, body: "<!DOCTYPE html> ...", headers: ... }
```

## गहरी जाँच:
HTTP वेबसाइट के अतिरिक्त अन्य विकल्प शामिल हैं। उनमें सबसे लोकप्रिय विकल्प जैसे GET, POST, PUT, DELETE आदि शामिल हैं। HTTP क्लाइंट लाइब्रेरी जैसे HTTPoison या आप अपने एपीआई उपयोगकर्ता को कुछ विशेष प्रकार के डेटा जैसे JSON या XML में भेज सकते हैं।

## आगे से देखें:
- [HTTPoison डॉक्यूमेंटेशन](https://hexdocs.pm/httpoison/readme.html)
- [एचटीटीपी हाइपर टॉरशन स्पेस](https://httpstatus.es/)
- [HTTP क्लाइंट लाइब्रेरी की तुलना](https://github.com/staskobzar/http-comparison)