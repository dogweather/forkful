---
title:                "HTTP अनुरोध भेजना"
aliases: - /hi/elixir/sending-an-http-request.md
date:                  2024-01-20T18:00:10.787370-07:00
model:                 gpt-4-1106-preview
simple_title:         "HTTP अनुरोध भेजना"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/sending-an-http-request.md"
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
