---
date: 2024-01-20 17:44:32.115145-07:00
description: "\u0935\u0947\u092C \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\
  \u094B\u0921 \u0915\u0930\u0928\u093E \u0938\u0930\u0932 \u0939\u0948: \u0906\u092A\
  \ \u090F\u0915 \u0935\u0947\u092C\u0938\u093E\u0907\u091F \u0938\u0947 \u0938\u0942\
  \u091A\u0928\u093E \u092A\u094D\u0930\u093E\u092A\u094D\u0924 \u0915\u0930\u0924\
  \u0947 \u0939\u0948\u0902\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\
  \u092E\u0930\u094D\u0938 \u092F\u0939 \u0915\u093E\u0930\u094D\u092F \u0906\u0901\
  \u0915\u0921\u093C\u0947 \u0907\u0915\u0920\u094D\u0920\u093E \u0915\u0930\u0928\
  \u0947, \u0938\u0947\u0935\u093E\u0913\u0902 \u0915\u093E \u092E\u093F\u0932\u093E\
  \u0928 \u0915\u0930\u0928\u0947, \u092F\u093E \u0938\u094D\u0935\u0924: \u0905\u092A\
  \u0921\u0947\u091F\u2026"
lastmod: 2024-02-19 22:05:10.798376
model: gpt-4-1106-preview
summary: "\u0935\u0947\u092C \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\u094B\
  \u0921 \u0915\u0930\u0928\u093E \u0938\u0930\u0932 \u0939\u0948: \u0906\u092A \u090F\
  \u0915 \u0935\u0947\u092C\u0938\u093E\u0907\u091F \u0938\u0947 \u0938\u0942\u091A\
  \u0928\u093E \u092A\u094D\u0930\u093E\u092A\u094D\u0924 \u0915\u0930\u0924\u0947\
  \ \u0939\u0948\u0902\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\
  \u0930\u094D\u0938 \u092F\u0939 \u0915\u093E\u0930\u094D\u092F \u0906\u0901\u0915\
  \u0921\u093C\u0947 \u0907\u0915\u0920\u094D\u0920\u093E \u0915\u0930\u0928\u0947\
  , \u0938\u0947\u0935\u093E\u0913\u0902 \u0915\u093E \u092E\u093F\u0932\u093E\u0928\
  \ \u0915\u0930\u0928\u0947, \u092F\u093E \u0938\u094D\u0935\u0924: \u0905\u092A\u0921\
  \u0947\u091F\u2026"
title: "\u0935\u0947\u092C \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\u094B\
  \u0921 \u0915\u0930\u0928\u093E"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
वेब पेज डाउनलोड करना सरल है: आप एक वेबसाइट से सूचना प्राप्त करते हैं। प्रोग्रामर्स यह कार्य आँकड़े इकठ्ठा करने, सेवाओं का मिलान करने, या स्वत: अपडेट प्राप्त करने के लिए करते हैं।

## How to: (कैसे करें:)
Elixir में, `HTTPoison` लाइब्रेरी का प्रयोग कर वेब पेज डाउनलोड करना आसां होता है। इसे `mix.exs` में निम्नलिखित कोड के साथ जोड़ें:

```elixir
defp deps do
  [
    {:httpoison, "~> 1.8"}
  ]
end
```

आवश्यकतानुसार सेटअप करने के बाद, वेब पेज को डाउनलोड करने के लिए:

```elixir
defmodule PageDownloader do
  def download(url) do
    case HTTPoison.get(url) do
      {:ok, %HTTPoison.Response{status_code: 200, body: body}} ->
        {:ok, body}
      {:ok, %HTTPoison.Response{status_code: status_code}} ->
        {:error, "Failed with status code: #{status_code}"}
      {:error, %HTTPoison.Error{reason: reason}} ->
        {:error, reason}
    end
  end
end

# सैंपल आउटपुट
PageDownloader.download("http://example.com")
# {:ok, "<html>...</html>"}
```

## Deep Dive (गहरी जानकारी)
इलिक्सिर के अन्दर वेब पेज डाउनलोड करने का इतिहास नया है, लेकिन इसका आधार Erlang के HTTP क्लाइंट पर है। `HTTPoison` इस्तेमाल करने का विकल्प `HTTPotion` या Erlang के `:httpc` मॉड्यूल हैं। `HTTPoison` का प्रयोग इसकी आसान API और सुविधाजनक तरीकों की वजह से किया जाता है। इस लाइब्रेरी में एसिंक्रोनस अनुरोध और प्लगइंस जैसी कई सशक्त क्षमताएँ होती हैं।

## See Also (और भी देखें)
- HTTPoison GitHub Repo: https://github.com/edgurgel/httpoison
- Elixir Document सरकारी साइट: https://elixir-lang.org/docs.html
- Erlang `:httpc` मॉड्यूल: http://erlang.org/doc/man/httpc.html
