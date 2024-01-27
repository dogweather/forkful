---
title:                "CSV के साथ काम करना"
date:                  2024-01-19
html_title:           "Bash: CSV के साथ काम करना"
simple_title:         "CSV के साथ काम करना"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/working-with-csv.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
CSV (Comma-Separated Values) एक साधारण फ़ाइल प्रारूप है जो तालिका जैसे डेटा को संग्रहित करता है। यह डेटा को आसानी से स्थानांतरित, पढ़ने और लिखने के लिए प्रोग्रामर्स द्वारा प्रयोग किया जाता है।

## How to: (कैसे करें:)
Elixir में CSV पढ़ने और लिखने के लिए, हम `CSV` लाइब्रेरी का उपयोग कर सकते हैं।

```elixir
# CSV डाटा पढ़ना
defmodule CSVExample do
  alias NimbleCSV.RFC4180, as: CSV

  def read_csv(file_path) do
    file_path
    |> File.stream!()
    |> CSV.parse_stream()
    |> Enum.to_list()
  end

  # CSV फ़ाइल लिखना
  def write_csv(file_path, data) do
    # डेटा को CSV स्ट्रिंग में बदलें और फ़ाइल में लिखें
    CSV.dump_to_stream(data)
    |> Stream.into(File.stream!(file_path, [:write]))
    |> Stream.run()
  end
end

# उदाहरण के डाटा से CSV फाइल बनाना और पढ़ना
data = [["name", "age"], ["Alice", 30], ["Bob", 28]]

# CSV फाइल लिखें
CSVExample.write_csv("people.csv", data)

# CSV फाइल पढ़ें
IO.inspect CSVExample.read_csv("people.csv")
```

ऊपर कोड चलाने पर, `people.csv` फ़ाइल बनेगी और उसका आउटपुट दिखाई देगा जिसमें नाम और उम्र के साथ एक तालिका होगी।

## Deep Dive (गहराई में जानकारी)
CSV लंबे समय से डेटा इंटरचेंज के लिए एक मानक प्रारूप रहा है। इसे विभिन्न भाषाओं और प्रोग्रामिंग टूल्स द्वारा बिना किसी अतिरिक्त डिपेंडेंसी के समर्थन किया गया है। Elixir में `NimbleCSV` एक बहुत ही प्रभावी लाइब्रेरी है जो CSV में संग्रहित डेटा के पढ़ने-लिखने को संभालती है। वैकल्पिक रूप से, कई अन्य फार्मेट्स जैसे कि JSON, XML, या यहाँ तक कि डाटाबेस सिस्टम्स भी डेटा स्टोरेज और ट्रांसफर के लिए उपयोग होते हैं, पर CSV इसकी सहजता के कारण अभी भी बहुत पसंद किया जाता है। 

## See Also (अन्य संसाधन)
- Elixir के लिए `NimbleCSV` लाइब्रेरी का डाक्यूमेंटेशन: [https://hexdocs.pm/nimble_csv](https://hexdocs.pm/nimble_csv)
- Elixir प्रोग्रामिंग के बारे में अधिक जानकारी: [https://elixir-lang.org/](https://elixir-lang.org/)
- CSV RFC4180 स्टैंडर्ड: [https://tools.ietf.org/html/rfc4180](https://tools.ietf.org/html/rfc4180)
