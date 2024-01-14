---
title:                "Elixir: पैटर्न को मेल खाते हुए अक्षरों को हटाना"
simple_title:         "पैटर्न को मेल खाते हुए अक्षरों को हटाना"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

देरी क्यों?

कोई भी प्रोग्रामिंग भाषा में डाटा मनुपलेशन का सबसे अधिक प्रयोग किया जाता है। इक्सिर भी डाटा संवाद में उपयोगी है। इसमें डाटा प्रोसेसिंग के लिए कई साधन हैं। इसमें एक महत्वपूर्ण उपकरण है कि आप किसी भी पैटर्न से मिलते हुए वर्ण हटा सकते हो। 

कैसे करें?

यह मुश्किल नहीं है। आप बस विशेष वर्णन के साथ स्ट्रिंग विधि का उपयोग करके उपनिर्धारण से बचने के लिए स्ट्रिंग से दुनिया में सरे दिन हो लिए है।

```Elixir
string = "इक्सिर भाषा में"
string |> String.replace("ा", "") # इक्सिर भसभ में
```

गहरी गड़ही

चेरदी करिक के समान चेरदी से जुड़े निर्देशांक के प्राप्ति से सम्बन्धित भगुदी के बराबर लोग ऐसा कर सकते हैं। आप वे वर्ण को भी नहीं हटा सकेंगे।

देखिए भी

- [इक्सिर डाटा संवाद: मांग और मिलान](https://www.bogotobogo.com/Elixir/Elixir_Data_Structures.php)
- [इक्सिर भाषा भाषा लेख के लिए वर्गीकरण](http://elixir-lang.org/getting-started/basic-types.html)