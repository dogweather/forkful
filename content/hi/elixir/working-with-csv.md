---
title:                "Elixir: कम्प्यूटर प्रोग्रामिंग में csv के साथ काम करना"
simple_title:         "कम्प्यूटर प्रोग्रामिंग में csv के साथ काम करना"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/working-with-csv.md"
---

{{< edit_this_page >}}

## क्यों

इंटरनेट और सॉफ्टवेयर विकास का समय और प्रयास बचाने के लिए, हम अक्सर डेटा को CSV (कॉमा से अलगिंगित सत्यापित) फ़ॉर्मेट में संग्रहीत करते हैं। इस ब्लॉग पोस्ट में हम जानेंगे कि Elixir में CSV संग्रहीत में कैसे काम करता है और इसमें सामान्य समस्याओं को कैसे हल किया जाता है।

## कैसे करें

अगर आपने Elixir का इस्तेमाल नहीं किया है, तो पहले आधिकारिक वेबसाइट पर जाकर इसे डाउनलोड करें। CSV संग्रहीत को मैनुअली पढ़ने के लिए आपको `File.stream!` फ़ंक्शन के साथ `CSV.new` का उपयोग करना होगा। मूल्यों को आमतौर पर सपेटेट करने के लिए, हमें संशोधित फ़ंक्शन `|> with_delimiter` का उपयोग करना होगा। नीचे दी गई उदाहरण में, हमें अलग शीर्षक दिए गए तीन स्तंभों `name`, `age` और `email` से एक CSV फ़ाइल को संवर्धित करना होगा।

```Elixir
require CSV

File.stream!("example.csv") 
|> CSV.new(delimiter: ";") 
|> CSV.StreamRows 
|> Enum.to_list 
#=> [name: "John", age: 29, email: "john@example.com"]
```

## गहराई से जाएँ

इस उदाहरण के साथ, हमने `File.stream!` का उपयोग CSV संग्रहीत को संवर्धित करने के लिए किया है। इसमें `Enum.reload` का उपयोग `:header` पैरामीटर के साथ अनुरोधित रोज़ करने के लिए किया गया है। इससे हमें पहले से ही CSV स्तंभों को सही आदेश में प्राप्त करने में मदद मिलती है।

## और भी देखें

- [Elixir CSV डॉक्युमेंटेशन](https://hexdocs.pm/csv/CSV.html)
- [Elixir के साथ गणनांकित प्रो