---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:12:31.917044-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: Elixir, \u0905\u092A\
  \u0928\u0947 \u092E\u091C\u092C\u0942\u0924 \u0938\u092E\u0935\u0930\u094D\u0924\
  \u0940 \u092E\u0949\u0921\u0932 \u0914\u0930 \u0915\u093E\u0930\u094D\u092F\u093E\
  \u0924\u094D\u092E\u0915 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u093F\
  \u0902\u0917 \u092A\u0930\u093F\u092E\u093E\u0923 \u0915\u0947 \u0938\u093E\u0925\
  , \u092C\u093F\u0932\u094D\u0921-\u0907\u0928 HTML \u092A\u093E\u0930\u094D\u0938\
  \u093F\u0902\u0917 \u0915\u094D\u0937\u092E\u0924\u093E\u0913\u0902 \u0915\u094B\
  \ \u0936\u093E\u092E\u093F\u0932 \u0928\u0939\u0940\u0902 \u0915\u0930\u0924\u093E\
  \u0964 \u0939\u093E\u0932\u093E\u0902\u0915\u093F, \u0907\u0938\u2026"
lastmod: '2024-04-05T21:53:53.744475-06:00'
model: gpt-4-0125-preview
summary: "Elixir, \u0905\u092A\u0928\u0947 \u092E\u091C\u092C\u0942\u0924 \u0938\u092E\
  \u0935\u0930\u094D\u0924\u0940 \u092E\u0949\u0921\u0932 \u0914\u0930 \u0915\u093E\
  \u0930\u094D\u092F\u093E\u0924\u094D\u092E\u0915 \u092A\u094D\u0930\u094B\u0917\u094D\
  \u0930\u093E\u092E\u093F\u0902\u0917 \u092A\u0930\u093F\u092E\u093E\u0923 \u0915\
  \u0947 \u0938\u093E\u0925, \u092C\u093F\u0932\u094D\u0921-\u0907\u0928 HTML \u092A\
  \u093E\u0930\u094D\u0938\u093F\u0902\u0917 \u0915\u094D\u0937\u092E\u0924\u093E\u0913\
  \u0902 \u0915\u094B \u0936\u093E\u092E\u093F\u0932 \u0928\u0939\u0940\u0902 \u0915\
  \u0930\u0924\u093E\u0964 \u0939\u093E\u0932\u093E\u0902\u0915\u093F, \u0907\u0938\
  \ \u0909\u0926\u094D\u0926\u0947\u0936\u094D\u092F \u0915\u0947 \u0932\u093F\u090F\
  \ \u0906\u092A `Floki` \u091C\u0948\u0938\u0940 \u0932\u094B\u0915\u092A\u094D\u0930\
  \u093F\u092F \u0924\u0940\u0938\u0930\u0947 \u092A\u0915\u094D\u0937 \u0915\u0940\
  \ \u0932\u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940\u091C \u0915\u093E \u0909\
  \u092A\u092F\u094B\u0917 \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\
  \u0964 Floki, Elixir \u0915\u0947 \u092A\u0948\u091F\u0930\u094D\u0928 \u092E\u093F\
  \u0932\u093E\u0928 \u0914\u0930 \u092A\u093E\u0907\u092A\u093F\u0902\u0917 \u092B\
  \u0940\u091A\u0930\u094D\u0938 \u0915\u093E \u0932\u093E\u092D \u0909\u0920\u093E\
  \u0915\u0930, HTML \u092A\u093E\u0930\u094D\u0938\u093F\u0902\u0917 \u0915\u094B\
  \ \u0938\u0939\u091C \u0914\u0930 \u0915\u0941\u0936\u0932 \u092C\u0928\u093E\u0924\
  \u0940 \u0939\u0948\u0964 \u092A\u0939\u0932\u0947, \u0905\u092A\u0928\u0940 mix.exs\
  \ \u0928\u093F\u0930\u094D\u092D\u0930\u0924\u093E\u0913\u0902 \u092E\u0947\u0902\
  \ Floki \u091C\u094B\u0921\u093C\u0947\u0902."
title: "HTML \u0935\u093F\u0936\u094D\u0932\u0947\u0937\u0923"
weight: 43
---

## कैसे करें:
Elixir, अपने मजबूत समवर्ती मॉडल और कार्यात्मक प्रोग्रामिंग परिमाण के साथ, बिल्ड-इन HTML पार्सिंग क्षमताओं को शामिल नहीं करता। हालांकि, इस उद्देश्य के लिए आप `Floki` जैसी लोकप्रिय तीसरे पक्ष की लाइब्रेरीज का उपयोग कर सकते हैं। Floki, Elixir के पैटर्न मिलान और पाइपिंग फीचर्स का लाभ उठाकर, HTML पार्सिंग को सहज और कुशल बनाती है।

पहले, अपनी mix.exs निर्भरताओं में Floki जोड़ें:

```elixir
defp deps do
  [
    {:floki, "~> 0.31.0"}
  ]
end
```

फिर, नई निर्भरता स्थापित करने के लिए `mix deps.get` चलाएँ।

अब, आइए एक सरल HTML स्ट्रिंग को पार्स करें ताकि डेटा निकाल सकें। हम `<h1>` टैग्स के अंदर शीर्षकों को ढूंढेंगे:

```elixir
html_content = """
<html>
  <body>
    <h1>Hello, Elixir!</h1>
    <h1>Another Title</h1>
  </body>
</html>
"""

titles = html_content
         |> Floki.find("h1")
         |> Floki.text()

IO.inspect(titles)
```

**नमूना आउटपुट:**

```elixir
["Hello, Elixir!", "Another Title"]
```

और गहरा डूबने के लिए, मान लें कि आप साथ में href विशेषताओं के साथ लिंक्स (`<a>` टैग्स) निकालना चाहते हैं। यहाँ पर आप इसे कैसे हासिल कर सकते हैं:

```elixir
html_content = """
<html>
  <body>
    <a href="https://elixir-lang.org/">Elixir की आधिकारिक वेबसाइट</a>
    <a href="https://hexdocs.pm/">HexDocs</a>
  </body>
</html>
"""

links = html_content
        |> Floki.find("a")
        |> Enum.map(fn({_, attrs, [text]}) -> {text, List.keyfind(attrs, "href", 0)} end)
        
IO.inspect(links)
```

**नमूना आउटपुट:**

```elixir
[{"Elixir की आधिकारिक वेबसाइट", {"href", "https://elixir-lang.org/"}}, {"HexDocs", {"href", "https://hexdocs.pm/"}}]
```

यह दृष्टिकोण आपको HTML दस्तावेजों को कुशलतापूर्वक नेविगेट और पार्स करने देता है, Elixir एप्लिकेशंस में वेब डेटा निष्कर्षण और हेरफेर कार्यों को सरल बनाता है।
