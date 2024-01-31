---
title:                "JSON के साथ काम करना"
date:                  2024-01-19
html_title:           "Arduino: JSON के साथ काम करना"
simple_title:         "JSON के साथ काम करना"

tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/working-with-json.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
JSON (JavaScript Object Notation) एक हल्का फॉर्मेट है जिससे डेटा आदान-प्रदान में आसानी होती है। Elixir प्रोग्रामर्स JSON का उपयोग करते हैं क्योंकि यह ह्यूमन और मशीन दोनों के लिए पढ़ने में सरल होता है, और वेब APIs के साथ इंटरैक्शन में बहुत मदद करता है।

## How to: (कैसे करें:)
Elixir में JSON से काम करने के लिए, `jason` हेक्स पैकेज आमतौर पर उपयोग किया जाता है। नीचे कुछ उदाहरण हैं:

```elixir
# Jason पैकेज को अपने mix.exs फाइल में जोड़ें
defp deps do
  [
    {:jason, "~> 1.2"}
  ]
end

# JSON स्ट्रिंग को Elixir मैप में परिवर्तित करना
json_string = "{\"name\": \"Ajay\", \"age\": 30}"
{:ok, data} = Jason.decode(json_string)
IO.inspect(data)

# Elixir मैप को JSON स्ट्रिंग में परिवर्तित करना
data_to_encode = %{"name" => "Vijay", "age" => 25}
{:ok, json_output} = Jason.encode(data_to_encode)
IO.puts(json_output)
```

Sample Output:
```
%{"age" => 30, "name" => "Ajay"}
{"age":25,"name":"Vijay"}
```

## Deep Dive (गहराई से समझिए):
ऐतिहासिक रूप से, JSON को डग क्रॉकफोर्ड ने प्रोमोट किया, और यह XML का एक लाइटवेट अल्टरनेटिव बन गया। इलिक्सिर में, `poison` और `jsx` जैसे अन्य लाइब्रेरीज भी हैं जो JSON पार्सिंग सपोर्ट करते हैं। लेकिन `jason` तेज़ और मेंटेन करने में आसान होने के कारण पसंद की जाती है। इम्प्लीमेंटेशन में कोई Elixir डेटा स्ट्रक्चर को JSON में ट्रांसफॉर्म करता है और उल्टा भी।

## See Also (और देखें):
- Elixir के `jason` पैकेज का डॉक्युमेंटेशन: [https://hexdocs.pm/jason/](https://hexdocs.pm/jason/)
- JSON स्पेसिफिकेशन: [https://www.json.org/json-en.html](https://www.json.org/json-en.html)
