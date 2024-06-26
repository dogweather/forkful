---
date: 2024-01-20 18:01:41.787107-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: Elixir \u092E\u0947\
  \u0902 HTTPoison \u0932\u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940 \u0915\u093E\
  \ \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\u0947 HTTP \u0905\u0928\u0941\
  \u0930\u094B\u0927 \u092C\u0947\u0938\u093F\u0915 \u092A\u094D\u0930\u092E\u093E\
  \u0923\u0940\u0915\u0930\u0923 \u0915\u0947 \u0938\u093E\u0925 \u092D\u0947\u091C\
  \u093E \u091C\u093E \u0938\u0915\u0924\u093E \u0939\u0948\u0964 \u092A\u0939\u0932\
  \u0947 `HTTPoison` \u091C\u094B\u0921\u093C\u0947\u0902 \u0905\u092A\u0928\u0947\
  \ `mix.exs` \u092B\u093E\u0907\u0932\u2026"
lastmod: '2024-03-13T22:44:51.736167-06:00'
model: gpt-4-1106-preview
summary: "Elixir \u092E\u0947\u0902 HTTPoison \u0932\u093E\u0907\u092C\u094D\u0930\
  \u0947\u0930\u0940 \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\
  \u0947 HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092C\u0947\u0938\u093F\u0915\
  \ \u092A\u094D\u0930\u092E\u093E\u0923\u0940\u0915\u0930\u0923 \u0915\u0947 \u0938\
  \u093E\u0925 \u092D\u0947\u091C\u093E \u091C\u093E \u0938\u0915\u0924\u093E \u0939\
  \u0948\u0964 \u092A\u0939\u0932\u0947 `HTTPoison` \u091C\u094B\u0921\u093C\u0947\
  \u0902 \u0905\u092A\u0928\u0947 `mix.exs` \u092B\u093E\u0907\u0932 \u092E\u0947\u0902\
  \u0964."
title: "\u092C\u0947\u0938\u093F\u0915 \u092A\u094D\u0930\u092E\u093E\u0923\u0940\u0915\
  \u0930\u0923 \u0915\u0947 \u0938\u093E\u0925 HTTP \u0905\u0928\u0941\u0930\u094B\
  \u0927 \u092D\u0947\u091C\u0928\u093E"
weight: 45
---

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
