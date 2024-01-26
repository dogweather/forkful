---
title:                "HTML पार्स करना"
date:                  2024-01-20T15:32:09.383853-07:00
html_title:           "Bash: HTML पार्स करना"
simple_title:         "HTML पार्स करना"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
HTML पार्सिंग एक प्रक्रिया है जिसमें HTML डॉक्यूमेंट को विश्लेषण करके उसके डाटा को ढूंढा और पुनः प्राप्त किया जाता है। प्रोग्रामर इसे वेब पेजों से जानकारी निकालने, ऑटोमेशन और डेटा प्रोसेसिंग कार्यों के लिए करते हैं।

## How to: (कैसे करें:)
Elixir में HTML पार्सिंग के लिए हम `Floki` लाइब्रेरी का उपयोग करते हैं। यह एक सिंपल एवं पावरफुल HTML XML पार्सिंग लाइब्रेरी है। सबसे पहले, `Floki` को mix.exs में dependency के रूप में जोड़ें:

```elixir
defp deps do
  [
    {:floki, "~> 0.30.0"}
  ]
end
```

अब, आइए कुछ HTML कंटेंट पार्स करते हैं:

```elixir
html = "<html><body><p>Hello, Elixir!</p></body></html>"
{:ok, document} = Floki.parse_document(html)
paragraphs = Floki.find(document, "p")
text = Enum.map(paragraphs, &Floki.text(&1))
IO.inspect text  # Output होगा: ["Hello, Elixir!"]
```

## Deep Dive (गहराई में जानकारी):
Elixir का इस्तेमाल करते हुए HTML पार्सिंग आमतौर पर NIFs (Native Implemented Functions) का उपयोग करके एक्जीक्यूट होती है जो कि एर्लांग को दूसरी प्रोग्रामिंग भाषाओं के कोड के साथ इंटरऑपरेबल बनाता है। जैसे कि `Floki` आंतरिक रूप से मोकसा (Mochiweb) के `mochiweb_html` मॉड्यूल को उपयोग में लेता है। `Floki` से पहले, `Mochiweb` और `Erlang's Xmerl` जैसे अल्टरनेटिव भी प्रचलित थे।

इम्प्लीमेंटेशन विवरण में, `Floki` क्वेरी सलेक्टर्स को `Elixir` मैप्स और लिस्ट्स में बदल देता है जिसे आसानी से मैन्युपुलेट किया जा सकता है। यह एक DOM-like स्ट्रक्चर में HTML को पार्स करता है, जिससे परिचित वेब डेवलपर्स के लिए यूज करना सुविधाजनक होता है।

## See Also (यह भी देखें):
- [Floki GitHub](https://github.com/philss/floki) - `Floki` का सोर्स कोड और डॉक्यूमेंटेशन।
- [Erlang Xmerl](http://erlang.org/doc/apps/xmerl/) - Erlang का `Xmerl` मॉड्यूल जो XML पार्सिंग के लिए उपयोग में लिया जाता है।
