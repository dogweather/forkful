---
title:                "बेसिक प्रमाणीकरण के साथ HTTP अनुरोध भेजना"
aliases: - /hi/elixir/sending-an-http-request-with-basic-authentication.md
date:                  2024-01-20T18:01:41.787107-07:00
model:                 gpt-4-1106-preview
simple_title:         "बेसिक प्रमाणीकरण के साथ HTTP अनुरोध भेजना"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

HTTP अनुरोध बेसिक प्रमाणीकरण के साथ भेजना एक प्रक्रिया है जिसमें वेब सेवा प्रमाणित करती है कि अनुरोध भेजने वाला उस डेटा तक पहुँचने का हकदार है। प्रोग्रामर इसका उपयोग अक्सर सिक्योर API एंडपॉइंट्स तक पहुँचने के लिए करते हैं।

## कैसे करें:

Elixir में HTTPoison लाइब्रेरी का उपयोग करके HTTP अनुरोध बेसिक प्रमाणीकरण के साथ भेजा जा सकता है। पहले `HTTPoison` जोड़ें अपने `mix.exs` फाइल में।

```elixir
def deps do
  [
    {:httpoison, "~> 1.8"}
  ]
end
```

फिर निम्न कोड का उपयोग करें:

```elixir
defmodule MyHTTPClient do
  def send_request do
    auth = {"my_username", "my_password"}
    options = [basic_auth: auth]
    HTTPoison.get!("https://example.com/secret-data", [], options)
  end
end
```

सैंपल आउटपुट:

```
%HTTPoison.Response{
  status_code: 200,
  body: "...",
  headers: [...],
}
```

## गहराई से जानकारी

HTTP बेसिक प्रमाणीकरण एक मानक तकनीक है जो 1990 के दशक से उपयोग में है। यह यूजरनेम और पासवर्ड को Base64 में एन्कोड करके HTTP हेडर (`Authorization`) में भेजती है। इस पद्धति का मुख्य विकल्प OAuth है, जिसे अधिक सुरक्षा के लिए इस्तेमाल किया जाता है। Elixir में, `HTTPoison` के अलावा, आप `Tesla` या `hackney` जैसे अन्य HTTP क्लाइंट्स भी इस्तेमाल कर सकते हैं।

## संबंधित स्रोत

- [HTTPoison GitHub repository](https://github.com/edgurgel/httpoison)
- [MDN Web Docs: Authorization](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Authorization)
