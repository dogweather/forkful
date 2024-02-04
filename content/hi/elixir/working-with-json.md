---
title:                "JSON के साथ काम करना"
date:                  2024-02-03T19:22:49.042163-07:00
model:                 gpt-4-0125-preview
simple_title:         "JSON के साथ काम करना"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/working-with-json.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?

JSON के साथ कार्य करना इसमें JSON-स्वरूपित स्ट्रिंग्स को डेटा संरचनाओं में पार्स करना शामिल है जिसे एलिक्सिर संचालित कर सकता है, और एलिक्सिर डेटा संरचनाओं को वापस JSON स्ट्रिंग्स में सीरियलाइज़ करना। यह वेब विकास, APIs, और कॉन्फ़िगरेशन फाइलों के लिए आवश्यक है, क्योंकि JSON एक हल्का, पाठ-आधारित, भाषा-स्वतंत्र डेटा एक्सचेंज स्वरूप है जो इसकी सादगी और मानव-पठनीयता के लिए व्यापक रूप से प्रयोग किया जाता है।

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
