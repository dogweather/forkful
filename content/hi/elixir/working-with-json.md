---
title:                "JSON के साथ काम करना"
html_title:           "Elixir: JSON के साथ काम करना"
simple_title:         "JSON के साथ काम करना"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Data Formats and Serialization"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/working-with-json.md"
---

{{< edit_this_page >}}

## क्यों

क्यों कर रहे हो आप JSON के साथ काम? अक्सर डेटा को स्टोर करने और प्रसंस्करण करने के लिए जब हम विभिन्न एप्लिकेशन और सिस्टमों के बीच डेटा को ट्रांसफर करते है, तो हम JSON का उपयोग करते है। इलिक्सिर में JSON का सबसे अधिक उपयोग डेटा एपीआई के साथ काम करने में होता है। 

## कैसे करें

टेक्स्ट को JSON में बदलने के लिए, `Jason.encode!/1` फंक्शन का उपयोग करें। इसके बाद, हम डेटा को वापस स्ट्रिंग में बदलने के लिए `Jason.encode_to_iodata!/1` फंक्शन का उपयोग कर सकते है। अगर हमारे पास एक स्ट्रिंग हो और हम उसे JSON में प्रकट करना चाहते है, तो हम `Jason.decode!/1` फंक्शन का उपयोग कर सकते है। नीचे दिए गए उदाहरण को रन करने के लिए [iex](https://hexdocs.pm/iex/IEx.html#start/0)` टूल का उपयोग कर सकते है।

```Elixir
# स्ट्रिंग को JSON में बदलने का उदाहरण
input = %{name: "John Doe", age: 30}
JSON.encode!(input)

# डेटा को स्ट्रिंग में बदलने का उदाहरण
input = %{name: "John Doe", age: 30}
JSON.encode_to_iodata!(input)

# स्ट्रिंग को JSON में प्रकट करने का उदाहरण
input = "{\"name\": \"John Doe\", \"age\": \"30\"}"
JSON.decode!(input)
```

## गहराई में जाएं

इलिक्सिर में, हम जेसन डेटा को वन एटा की शक्ति का उपयोग करके हैंडल करते है। यह डेटा एपीआई के साथ काम करने में बहुत ही उपयोगी है। इसके अलावा, इलिक्सिर परियोजनाओं में फेडरेशन सर्वर आदि के साथ काम करने के लिए भी अन्य लाइब्रेरियां उपलब्ध हैं