---
title:                "Elixir: Json के साथ काम करना"
simple_title:         "Json के साथ काम करना"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/working-with-json.md"
---

{{< edit_this_page >}}

## क्यों

JSON को समझना और काम करना एक महत्वपूर्ण टूल है जो कोडिंग को सफलतापूर्वक सम्पन्न करने में मदद करता है। Elixir में JSON विस्तृत तरीके से काम करता है और आनंददायक बनाता है।

## कैसे

JSON के साथ काम करने के लिए, हम `Poison` नामक Elixir का मॉड्यूल प्रयोग कर सकते हैं। इस मॉड्यूल के अंतर्गत एक `encode!` और `decode!` फंक्शन है जो आपको JSON में डेटा को संगठित करने और उसे स्ट्रिंग में बदलने में मदद करता है। यहां हम एक उदाहरण देख सकते हैं:

```
Elixir> data = %{language: "Hindi", level: "Advanced"}
%Elixir> encoded_data = Poison.encode!(data)
"{\"language\":\"Hindi\",\"level\":\"Advanced\"}"
%Elixir> decoded_data = Poison.decode!(encoded_data)
%{language: "Hindi", level: "Advanced"}
```

## गहराई में

JSON को एक स्ट्रिंग से Elixir डेटा स्ट्रक्चर में बदलने के लिए, `Poison` अंतर्गत `decode` फंक्शन का इस्तेमाल किया जाता है। इससे हम स्ट्रिंग को बाइनरी डेटा में बदल सकते हैं जो एक त्वरित प्रकार का संगठित डेटा होता है। इसके अलावा, एक JSON स्ट्रिंग को टाइपसैफ़ एतथीयट जैसे कि `floats` और `atoms` आदि के साथ भी इंटरफ़ेस किया जा सकता है।

## देखें भी

[JSON डॉक्यूमेंटेशन](https://hexdocs.pm/poison/readme.html#content)

[एक JSON स्ट्रिंग को इंटरफ़ेस और अन्य टाइपसैफ़ टाइप में बदलना](https://hexdocs.pm/poison/Poison.html#decode/2)

[एक Elixir मॉड्यूल में एक सुववे कोड स्निप्ट डेटा को JSON में संरचित करना](https://blog.heroku.com/elixir-release-process-and-json-web-encryption)

[सही तरह से JSON को पार्स और उन्मुक्त करना](https://hackernoon.com