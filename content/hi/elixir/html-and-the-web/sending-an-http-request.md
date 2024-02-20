---
date: 2024-01-20 18:00:10.787370-07:00
description: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u0928\u093E\
  \ \u0935\u0947\u092C \u0938\u0930\u094D\u0935\u0930 \u0938\u0947 \u091C\u093E\u0928\
  \u0915\u093E\u0930\u0940 \u092E\u093E\u0901\u0917\u0928\u0947 \u0915\u0940 \u090F\
  \u0915 \u092A\u094D\u0930\u0915\u094D\u0930\u093F\u092F\u093E \u0939\u0948\u0964\
  \ \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0921\
  \u0947\u091F\u093E \u092A\u094D\u0930\u093E\u092A\u094D\u0924 \u0915\u0930\u0928\
  \u0947, \u0938\u0947\u0935\u093E\u0913\u0902 \u0915\u0947 \u0938\u093E\u0925 \u0907\
  \u0902\u091F\u0930\u092B\u093C\u0947\u0938 \u0915\u0930\u0928\u0947, \u0914\u0930\
  \ \u0930\u093F\u092E\u094B\u091F \u0938\u0930\u094D\u0935\u0930\u094D\u0938 \u0915\
  \u0947 \u0938\u093E\u0925\u2026"
lastmod: 2024-02-19 22:05:10.794934
model: gpt-4-1106-preview
summary: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u0928\u093E\
  \ \u0935\u0947\u092C \u0938\u0930\u094D\u0935\u0930 \u0938\u0947 \u091C\u093E\u0928\
  \u0915\u093E\u0930\u0940 \u092E\u093E\u0901\u0917\u0928\u0947 \u0915\u0940 \u090F\
  \u0915 \u092A\u094D\u0930\u0915\u094D\u0930\u093F\u092F\u093E \u0939\u0948\u0964\
  \ \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0921\
  \u0947\u091F\u093E \u092A\u094D\u0930\u093E\u092A\u094D\u0924 \u0915\u0930\u0928\
  \u0947, \u0938\u0947\u0935\u093E\u0913\u0902 \u0915\u0947 \u0938\u093E\u0925 \u0907\
  \u0902\u091F\u0930\u092B\u093C\u0947\u0938 \u0915\u0930\u0928\u0947, \u0914\u0930\
  \ \u0930\u093F\u092E\u094B\u091F \u0938\u0930\u094D\u0935\u0930\u094D\u0938 \u0915\
  \u0947 \u0938\u093E\u0925\u2026"
title: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u0928\u093E"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

HTTP अनुरोध भेजना वेब सर्वर से जानकारी माँगने की एक प्रक्रिया है। प्रोग्रामर्स डेटा प्राप्त करने, सेवाओं के साथ इंटरफ़ेस करने, और रिमोट सर्वर्स के साथ बातचीत करने के लिए इसे करते हैं।

## How to: (कैसे करें:)

Elixir में HTTP अनुरोध भेजने के लिए `HTTPoison` लाइब्रेरी एक लोकप्रिय विकल्प है। पहले, `HTTPoison` ऐड करें:

```elixir
defp deps do
  [{:httpoison, "~> 1.8"}]
end
```

और उसे इंस्टॉल करने के लिए `mix deps.get` चलाएं।

अब एक सरल GET अनुरोध:

```elixir
defmodule HTTPExample do
  def fetch_data do
    HTTPoison.get("http://httpbin.org/get")
  end
end

case HTTPExample.fetch_data() do
  {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
    IO.inspect(body)
  {:error, %HTTPoison.Error{reason: reason}} ->
    IO.inspect(reason)
end
```

यह कोड httpbin.org से डेटा लेता है और उसे प्रिंट करता है।

## Deep Dive (गहराई से समझिए):

HTTP अनुरोध भेजने की क्षमता, Internet के उदय के साथ आई। `HTTPoison` जैसे लाइब्रेरी Elixir में इस कार्य को आसान बनाते हैं। इतिहास में, `HTTPotion` जैसे वैकल्पिक लाइब्रेरी भी प्रयोग की जाती थी, पर `HTTPoison` beaker ब्राउजर का उपयोग कर HTTP क्लाइंट के रूप में बेहतर प्रदर्शन करता है। `HTTPoison` `hackney` लाइब्रेरी के ऊपर बना है, जो कि एक HTTP क्लाइंट फॉर एर्लांग है। 

## See Also (इसे भी देखें):

- [HTTPoison GitHub repository](https://github.com/edgurgel/httpoison)
- [Erlang का `hackney` library](https://github.com/benoitc/hackney)
