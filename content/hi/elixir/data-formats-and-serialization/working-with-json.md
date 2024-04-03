---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:22:49.042163-07:00
description: "\u0915\u0948\u0938\u0947: \u090F\u0932\u093F\u0915\u094D\u0938\u093F\
  \u0930 \u092E\u0947\u0902, \u0906\u092A JSON \u092A\u093E\u0930\u094D\u0938\u093F\
  \u0902\u0917 \u0914\u0930 \u091C\u0947\u0928\u0930\u0947\u0936\u0928 \u0915\u0947\
  \ \u0932\u093F\u090F `Jason` \u092A\u0941\u0938\u094D\u0924\u0915\u093E\u0932\u092F\
  \ \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930 \u0938\u0915\u0924\u0947\
  \ \u0939\u0948\u0902, \u091C\u094B \u090F\u0915 \u0932\u094B\u0915\u092A\u094D\u0930\
  \u093F\u092F \u0935\u093F\u0915\u0932\u094D\u092A \u0939\u0948\u0964 \u0938\u092C\
  \u0938\u0947 \u092A\u0939\u0932\u0947, `mix.exs` \u092E\u0947\u0902 \u0905\u092A\
  \u0928\u0947\u2026"
lastmod: '2024-03-13T22:44:51.774189-06:00'
model: gpt-4-0125-preview
summary: "\u090F\u0932\u093F\u0915\u094D\u0938\u093F\u0930 \u092E\u0947\u0902, \u0906\
  \u092A JSON \u092A\u093E\u0930\u094D\u0938\u093F\u0902\u0917 \u0914\u0930 \u091C\
  \u0947\u0928\u0930\u0947\u0936\u0928 \u0915\u0947 \u0932\u093F\u090F `Jason` \u092A\
  \u0941\u0938\u094D\u0924\u0915\u093E\u0932\u092F \u0915\u093E \u0909\u092A\u092F\
  \u094B\u0917 \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902, \u091C\u094B\
  \ \u090F\u0915 \u0932\u094B\u0915\u092A\u094D\u0930\u093F\u092F \u0935\u093F\u0915\
  \u0932\u094D\u092A \u0939\u0948\u0964 \u0938\u092C\u0938\u0947 \u092A\u0939\u0932\
  \u0947, `mix.exs` \u092E\u0947\u0902 \u0905\u092A\u0928\u0947 \u092A\u094D\u0930\
  \u094B\u091C\u0947\u0915\u094D\u091F \u0915\u0940 \u0928\u093F\u0930\u094D\u092D\
  \u0930\u0924\u093E\u0913\u0902 \u092E\u0947\u0902 `Jason` \u091C\u094B\u0921\u093C\
  \u0947\u0902."
title: "JSON \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\
  \u093E"
weight: 38
---

## कैसे:
एलिक्सिर में, आप JSON पार्सिंग और जेनरेशन के लिए `Jason` पुस्तकालय का उपयोग कर सकते हैं, जो एक लोकप्रिय विकल्प है। सबसे पहले, `mix.exs` में अपने प्रोजेक्ट की निर्भरताओं में `Jason` जोड़ें:

```elixir
defp deps do
  [
    {:jason, "~> 1.3"}
  ]
end
```

फिर, निर्भरता फेच करने के लिए `mix deps.get` चलाएं।

### JSON पार्स करना:
एक JSON स्ट्रिंग को एलिक्सिर डेटा संरचनाओं में परिवर्तित करने के लिए:

```elixir
json_string = "{\"name\":\"John\", \"age\":30}"
{:ok, person} = Jason.decode(json_string)
IO.inspect(person)
# आउटपुट: %{"name" => "John", "age" => 30}
```

### JSON जेनरेट करना:
एक एलिक्सिर मैप को JSON स्ट्रिंग में परिवर्तित करने के लिए:

```elixir
person_map = %{"name" => "Jane", "age" => 25}
{:ok, json_string} = Jason.encode(person_map)
IO.puts(json_string)
# आउटपुट: {"age":25,"name":"Jane"}
```

### स्ट्रक्चर्स के साथ काम करना:
एक एलिक्सिर स्ट्रक्चर को एनकोड करने के लिए, आपको अपने स्ट्रक्चर के लिए `Jason.Encoder` प्रोटोकॉल को लागू करना होगा। यहाँ एक उदाहरण है:

```elixir
defmodule Person do
  @derive {Jason.Encoder, only: [:name, :age]}
  defstruct name: nil, age: nil
end

person_struct = %Person{name: "Mike", age: 28}
{:ok, json_string} = Jason.encode(person_struct)
IO.puts(json_string)
# आउटपुट: {"age":28,"name":"Mike"}
```

यह सरल दृष्टिकोण आपको विभिन्न प्रोग्रामिंग वातावरणों में डेटा आदान-प्रदान की सुविधा प्रदान करते हुए आपके एलिक्सिर अनुप्रयोगों में JSON प्रक्रिया को एकीकृत करने पर शुरू करेगा।
