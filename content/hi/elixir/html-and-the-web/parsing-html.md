---
title:                "HTML विश्लेषण"
aliases:
- /hi/elixir/parsing-html.md
date:                  2024-02-03T19:12:31.917044-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTML विश्लेषण"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

Elixir में HTML पार्सिंग का अर्थ है HTML दस्तावेजों से जानकारी निकालना। प्रोग्रामर इसे वेब पेजों के साथ प्रोग्रामैटिक रूप से बातचीत करने, डेटा स्क्रैप करने या वेब इंटरैक्शंस को ऑटोमेट करने के लिए करते हैं, जिससे एप्लिकेशंस वेब सामग्री को गतिशील रूप से समझ और उपयोग कर सकें।

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
