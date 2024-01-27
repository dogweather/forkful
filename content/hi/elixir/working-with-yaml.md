---
title:                "यामल के साथ काम करना"
date:                  2024-01-19
html_title:           "C#: यामल के साथ काम करना"
simple_title:         "यामल के साथ काम करना"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/working-with-yaml.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
YAML एक डाटा सीरियलाइजेशन फॉर्मेट है जो क्यूरेटेड डाटा प्रकारों को संग्रहित और साझा करने के लिए इस्तेमाल होता है। Elixir में YAML के साथ काम करने की जरूरत प्रोजेक्ट कॉन्फिगरेशन, डाटा एक्सचेंज और विभिन्न सर्विसेज के साथ इंटीग्रेशन के लिए पड़ती है।

## How to: (कैसे करें:)
Elixir में YAML के साथ काम करने के लिए `yamerl` लाइब्रेरी का इस्तेमाल करें।

Elixir में YAML पढ़ने के लिए:
```elixir
# Add yamerl as a dependency in mix.exs

def deps do
  [
    {:yamerl, "~> 0.8.0"}
  ]
end

# Use YamlElixir to parse the YAML data

defmodule MyYAML do
  def parse_yaml do
    yaml_data = """
    ---
    - Elixir
    - Phoenix
    - Nerves
    """

    {:ok, yamerl_conformant} = :yamerl_conform.documents(yaml_data)
    {:ok, Enum.map(yamerl_conformant, &(&1[:content]))}
  end
end

IO.inspect(MyYAML.parse_yaml)
```
नमूना आउटपुट होगा:
```
{:ok, [["Elixir", "Phoenix", "Nerves"]]}
```

Elixir में YAML लिखने के लिए:
Elixir में सीधे YAML लिखने के लिए कोई मूल लाइब्रेरी नहीं है, लेकिन आप JSON में कन्वर्ट कर फिर YAML में लिख सकते हैं।

## Deep Dive (विस्तार से):
YAML, जिसका पूरा नाम "YAML Ain't Markup Language" है, 2001 में विकसित किया गया था। यह JSON और XML के लिए एक सरल विकल्प के रूप में तैयार किया गया था। Elixir में, जहां देशीय सपोर्ट की कमी होती है, `yamerl` और `YamlElixir` लाइब्रेरीज का इस्तेमाल आमतौर पर YAML संबंधित कामों के लिए किया जाता है। YAML के प्रसार के पीछे इसका रीडेबल सिंटेक्स और डेटा-सेंट्रिक एप्लीकेशन्स के लिए सामर्थ्य है। एल्टनेटिव फॉर्मेट्स के तौर पर TOML और JSON भी लोकप्रिय हैं।

## See Also (और देखें):
- YAML official website for documentation: [YAML](https://yaml.org/)
- GitHub repository for `YamlElixir`: [YamlElixir on GitHub](https://github.com/KamilLelonek/yaml-elixir)
