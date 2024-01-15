---
title:                "स्ट्रिंग को लोअर केस में रूपांतरित करना"
html_title:           "Elixir: स्ट्रिंग को लोअर केस में रूपांतरित करना"
simple_title:         "स्ट्रिंग को लोअर केस में रूपांतरित करना"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# क्यों

अधिकतर बातचीत में, हमें स्ट्रिंग को लोअर केस में बदलने की जरूरत होती है। इससे न केवल डेटा की शुद्धता बनी रहती है, बल्कि विभिन्न विषाणु संचार के लिए आमसमान हो जाने का भी फायदा होता है।

# कैसे करें

यदि हमारे पास एक व्यक्ति शब्द (string) है तो उसे लोअर केस में बदलने के लिए, हम ```String.downcase/1``` का निर्देशन कर सकते हैं। इसकी उपयोग तरीका निम्नानुसार है:

```Elixir
String.downcase("HELLO") #=> "hello"
```

अगर हम उसी तरीके से अन्य डेटा पर्याप्त लंबाई तक (जिसमें से हमें ऐसी वस्तु नहीं मिलती है) अन्य डेटा को बदलने के लिए ```String.downcase_at/1``` का इस्तेमाल कर सकते हैं।

```Elixir
String.downcase_at("HELLO", 0) #=> "hello"
```

# गहराई तक

जब आप एक किसी भी स्ट्रिंग को लोअर केस में बदलते हैं, तो आप वास्तव में एक नया स्ट्रिंग बना देते हैं। यह वास्तविक आधारवादी समस्याओं के लिए प्रभावी नहीं है, लेकिन यह हमारे उदाहरण (examples) पर तो असर डाल सकता है। इस बात को समझने के लिए, आप हमारे इस लेख को पढ़ सकते हैं।

https://hexdocs.pm/elixir/String.html#downcase/2

# इसके अलावा देखें

- [String विन्यास](https://hexdocs.pm/elixir/String.html)
- [बार-बार चलाना](https://hexdocs.pm/elixir/String.html#downcase!/1)