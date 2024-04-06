---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:20:35.426243-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: Elixir, \u0905\u092A\
  \u0928\u0940 \u0936\u0915\u094D\u0924\u093F\u0936\u093E\u0932\u0940 \u092A\u0948\
  \u091F\u0930\u094D\u0928 \u092E\u093F\u0932\u093E\u0928 \u0914\u0930 \u092A\u093E\
  \u0907\u092A\u0932\u093E\u0907\u0928\u093F\u0902\u0917 \u0915\u0947 \u0938\u092E\
  \u0930\u094D\u0925\u0928 \u0915\u0947 \u0938\u093E\u0925, \u0924\u0940\u0938\u0930\
  \u0947 \u092A\u0915\u094D\u0937 \u0915\u0940 \u0932\u093E\u0907\u092C\u094D\u0930\
  \u0947\u0930\u093F\u092F\u094B\u0902 \u0915\u0947 \u092C\u093F\u0928\u093E \u092D\
  \u0940 CSV \u092B\u093E\u0907\u0932\u094B\u0902 \u0915\u094B \u0915\u0941\u0936\u0932\
  \u0924\u093E\u092A\u0942\u0930\u094D\u0935\u0915 \u0938\u0902\u092D\u093E\u0932\u2026"
lastmod: '2024-03-13T22:44:51.775915-06:00'
model: gpt-4-0125-preview
summary: "Elixir, \u0905\u092A\u0928\u0940 \u0936\u0915\u094D\u0924\u093F\u0936\u093E\
  \u0932\u0940 \u092A\u0948\u091F\u0930\u094D\u0928 \u092E\u093F\u0932\u093E\u0928\
  \ \u0914\u0930 \u092A\u093E\u0907\u092A\u0932\u093E\u0907\u0928\u093F\u0902\u0917\
  \ \u0915\u0947 \u0938\u092E\u0930\u094D\u0925\u0928 \u0915\u0947 \u0938\u093E\u0925\
  , \u0924\u0940\u0938\u0930\u0947 \u092A\u0915\u094D\u0937 \u0915\u0940 \u0932\u093E\
  \u0907\u092C\u094D\u0930\u0947\u0930\u093F\u092F\u094B\u0902 \u0915\u0947 \u092C\
  \u093F\u0928\u093E \u092D\u0940 CSV \u092B\u093E\u0907\u0932\u094B\u0902 \u0915\u094B\
  \ \u0915\u0941\u0936\u0932\u0924\u093E\u092A\u0942\u0930\u094D\u0935\u0915 \u0938\
  \u0902\u092D\u093E\u0932 \u0938\u0915\u0924\u0940 \u0939\u0948\u0964 \u0939\u093E\
  \u0932\u093E\u0901\u0915\u093F, \u0905\u0927\u093F\u0915 \u0909\u0928\u094D\u0928\
  \u0924 \u0906\u0935\u0936\u094D\u092F\u0915\u0924\u093E\u0913\u0902 \u0915\u0947\
  \ \u0932\u093F\u090F, `nimble_csv` \u0932\u093E\u0907\u092C\u094D\u0930\u0947\u0930\
  \u0940 \u090F\u0915 \u0924\u0947\u091C\u093C \u0914\u0930 \u0938\u0930\u0932 \u0935\
  \u093F\u0915\u0932\u094D\u092A \u0939\u0948\u0964\n\nElixir \u0915\u0947 \u092C\u093F\
  \u0932\u094D\u091F-\u0907\u0928 \u092B\u0902\u0915\u094D\u0936\u0928\u094D\u0938\
  \ \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\u0947 \u0906\u092A\
  \ \u090F\u0915 CSV \u092B\u093E\u0907\u0932 \u092A\u0922\u093C \u0914\u0930 \u092A\
  \u093E\u0930\u094D\u0938 \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902\
  ."
title: "CSV \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\u093E"
weight: 37
---

## कैसे करें:
Elixir, अपनी शक्तिशाली पैटर्न मिलान और पाइपलाइनिंग के समर्थन के साथ, तीसरे पक्ष की लाइब्रेरियों के बिना भी CSV फाइलों को कुशलतापूर्वक संभाल सकती है। हालाँकि, अधिक उन्नत आवश्यकताओं के लिए, `nimble_csv` लाइब्रेरी एक तेज़ और सरल विकल्प है।

### बिना बाह्य लाइब्रेरियों के CSV फाइल पढ़ना
Elixir के बिल्ट-इन फंक्शन्स का उपयोग करके आप एक CSV फाइल पढ़ और पार्स कर सकते हैं:

```elixir
defmodule CSVReader do
  def read_file(file_path) do
    File.stream!(file_path)
    |> Stream.map(&String.trim_trailing/1)
    |> Stream.map(&String.split(&1, ","))
    |> Enum.to_list()
  end
end

# नमूना प्रयोग
CSVReader.read_file("data.csv")
# आउटपुट: [["हैडर1", "हैडर2"], ["पंक्ति1मान1", "पंक्ति1मान2"], ["पंक्ति2मान1", "पंक्ति2मान2"]]
```

### एक CSV फाइल में लिखना
इसी तरह, एक CSV फाइल में डेटा लिखने के लिए:

```elixir
defmodule CSVWriter do
  def write_to_file(file_path, data) do
    File.open(file_path, [:write], fn file ->
      Enum.each(data, fn row ->
        IO.write(file, Enum.join(row, ",") <> "\n")
      end)
    end)
  end
end

# नमूना प्रयोग
data = [["हैडर1", "हैडर2"], ["मान1", "मान2"], ["मान3", "मान4"]]
CSVWriter.write_to_file("output.csv", data)
# Creates output.csv with the data formatted as CSV
```

### `nimble_csv` का उपयोग करना
अधिक जटिल CSV संभालने के लिए, `nimble_csv` CSV डेटा के साथ काम करने का एक शक्तिशाली और लचीला तरीका प्रदान करता है। पहले, अपने `mix.exs` में `nimble_csv` को अपनी निर्भरताओं में जोड़ें और `mix deps.get` चलाएं:

```elixir
defp deps do
  [
    {:nimble_csv, "~> 1.2"}
  ]
end
```

`nimble_csv` के साथ CSV डेटा पार्सिंग:

```elixir
defmodule MyCSVParser do
  NimbleCSV.define(MyParser, separator: ",", escape: "\\")

  def parse(file_path) do
    file_path
    |> File.stream!()
    |> MyParser.parse_stream()
    |> Enum.to_list()
  end
end

# नमूना प्रयोग
MyCSVParser.parse("data.csv")
# आउटपुट nimble_csv के साथ आपके पार्सर सेटअप पर निर्भर करता है, लेकिन यह आमतौर पर एक सूची के सूचियों या जोड़ों की तरह दिखता है।
```

`nimble_csv` का उपयोग करके CSV डेटा लिखना सही प्रारूप में अपने डेटा को मैन्युअली बदलने और फिर इसे एक फाइल में लिखने की आवश्यकता है, मूल Elixir उदाहरण की तरह, लेकिन सही प्रारूपित CSV पंक्तियों के उत्पादन के लिए `nimble_csv` का लाभ उठाना।

अपने कार्य की जटिलता के लिए उपयुक्त दृष्टिकोण का चयन करके, आप Elixir में CSV फाइलों को बहुत लचीलापन और शक्ति के साथ संभाल सकते हैं।
