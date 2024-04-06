---
date: 2024-01-20 18:00:10.787370-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Elixir\
  \ \u092E\u0947\u0902 HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\
  \u0928\u0947 \u0915\u0947 \u0932\u093F\u090F `HTTPoison` \u0932\u093E\u0907\u092C\
  \u094D\u0930\u0947\u0930\u0940 \u090F\u0915 \u0932\u094B\u0915\u092A\u094D\u0930\
  \u093F\u092F \u0935\u093F\u0915\u0932\u094D\u092A \u0939\u0948\u0964 \u092A\u0939\
  \u0932\u0947, `HTTPoison` \u0910\u0921 \u0915\u0930\u0947\u0902."
lastmod: '2024-04-05T21:53:53.742288-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Elixir \u092E\u0947\
  \u0902 HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u0928\u0947\
  \ \u0915\u0947 \u0932\u093F\u090F `HTTPoison` \u0932\u093E\u0907\u092C\u094D\u0930\
  \u0947\u0930\u0940 \u090F\u0915 \u0932\u094B\u0915\u092A\u094D\u0930\u093F\u092F\
  \ \u0935\u093F\u0915\u0932\u094D\u092A \u0939\u0948\u0964 \u092A\u0939\u0932\u0947\
  , `HTTPoison` \u0910\u0921 \u0915\u0930\u0947\u0902."
title: "HTTP \u0905\u0928\u0941\u0930\u094B\u0927 \u092D\u0947\u091C\u0928\u093E"
weight: 44
---

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
