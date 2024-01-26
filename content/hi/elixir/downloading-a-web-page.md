---
title:                "वेब पेज डाउनलोड करना"
date:                  2024-01-20T17:44:32.115145-07:00
model:                 gpt-4-1106-preview
simple_title:         "वेब पेज डाउनलोड करना"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/downloading-a-web-page.md"
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
