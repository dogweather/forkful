---
title:                "एक स्ट्रिंग से तारीख पार्स करना"
html_title:           "C++: एक स्ट्रिंग से तारीख पार्स करना"
simple_title:         "एक स्ट्रिंग से तारीख पार्स करना"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

एक स्ट्रिंग से डेट को पार्स करना यानी डेटा को उसके मूल तत्वों में विभाजित करना होता है, जैसे की दिन, महीना, और साल। प्रोग्रामर्स इसे करते हैं ताकि वे गठित डेटा को अधिक सुलभ और समझने योग्य तरीके से प्रबंधित कर सकें।

## कैसे: 

Elixir में स्ट्रिंग से डेट पार्स करने के लिए, `Date.from_iso8601/1` फ़ंक्शन का उपयोग करें। इसकी एक उदाहरण:

```Elixir
case Date.from_iso8601("2021-12-31") do
  {:ok, date} -> IO.inspect(date)
  {:error, reason} -> IO.puts("Error: #{reason}")
end
```
नतीजा इस ढ़ंग से दिखेगा:

```
~D[2021-12-31]
```

## गहराई से जानने के लिए: 

1. ऐतिहासिक सन्दर्भ: डेट पार्सिंग की कार्यवाही आस्थायी डेटा को अधिक स्थायी और स्थायी रूप में बदलने के लिए क्रियान्वित हुई है। यह सांख्यिकीय विश्लेषण के लिए विशेष रूप में महत्वपूर्ण हो सकता है।

2. विकल्प: Elixir के अलावा अन्य भाषाओं में भी डेट पार्स किए जा सकते हैं। JavaScript, Python और Ruby में इसके समरूपित फ़ंक्शन होते हैं। 

3. कार्यान्वयन विवरण: `Date.from_iso8601/1` एक विशेष ISO 8601 स्ट्रिंग को अपवादित करता है और यह निर्भर करता है कि स्ट्रिंग ISO 8601 तारीख प्रारूप के अनुसार होता है या नहीं। 

## अधिक जानने के लिए: 

ये कुछ संसाधन हैं जिनमें से आप अधिक जानकारी प्राप्त कर सकते हैं: 

1. Elixir का अधिकारिक दस्तावेज़ीकरण: [Date.from_iso8601/1](https://hexdocs.pm/elixir/Date.html#from_iso8601/1)
2. Date and Time in Elixir: [Elixir School](https://elixirschool.com/en/lessons/basics/date-time/)
3. काम करने का और व्यापक तरीका: [ISO 8601 Wikipedia](https://en.wikipedia.org/wiki/ISO_8601)