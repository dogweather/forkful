---
title:                "स्ट्रिंग को कैपिटलाइज़ करना"
date:                  2024-01-19
html_title:           "C: स्ट्रिंग को कैपिटलाइज़ करना"
simple_title:         "स्ट्रिंग को कैपिटलाइज़ करना"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)

स्ट्रिंग्स को कैपिटलाइज़ करने का मतलब है प्रत्येक शब्द की पहली अक्षर को बड़ा (कैपिटल लेटर) करना। प्रोग्रामर्स यह तब करते हैं जब उन्हें यूजर इंटरफ़ेस या डॉक्युमेंट्स में खास नामों या टाइटल्स को साफ और स्पष्ट दिखाना होता है।

## कैसे करें? (How to:)

```elixir
defmodule StringHelper do
  def capitalize_string(str) when is_binary(str) do
    str
    |> String.split()
    |> Enum.map(&String.capitalize/1)
    |> Enum.join(" ")
  end
end

StringHelper.capitalize_string("नमस्ते, यह एक टेस्ट स्ट्रिंग है।")
```

सैंपल आउटपुट: `"नमस्ते, यह एक टेस्ट स्ट्रिंग है।"`

## गहराई से जानकारी (Deep Dive)

ईलिक्सिर में स्ट्रिंग को कैपिटलाइज़ करने की कार्यप्रणाली बहुत सरल है। `String.capitalize/1` फंक्शन हर शब्द के पहले अक्षर को बड़ा कर देता है। ऐतिहासिक संदर्भ में, यह कंसेप्ट पुरानी प्रोग्रामिंग भाषाओं से आया है। विकल्प के रूप में, कुछ प्रोग्रामिंग भाषाओं में अलग बिल्ट-इन फंक्शंस होते हैं पर ईलिक्सिर इसे बहुत आसान बनाता है। `Enum.map/2` के साथ सम्मिलन करके, हम पूरी स्ट्रिंग के हर शब्द को लूप कर सकते हैं।

## सम्बंधित स्रोत (See Also)

- [Elixir String Module Documentation](https://hexdocs.pm/elixir/String.html)
- [Elixir School: Strings](https://elixirschool.com/en/lessons/basics/strings/)
- [Programming Elixir 1.6 by Dave Thomas](https://pragprog.com/titles/elixir16/programming-elixir-1-6/)
