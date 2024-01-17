---
title:                "Patten को मैच करने वाले characters को हटाना"
html_title:           "Elixir: Patten को मैच करने वाले characters को हटाना"
simple_title:         "Patten को मैच करने वाले characters को हटाना"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
मैचिंग पैटर्न के साथ अक्षरों को हटाना क्या है, और क्यों प्रोग्रामर इसे करते हैं। 

जब प्रोग्रामरों को अपने कोड से अनावश्यक या अनुपयुक्त अक्षरों को हटाना होता है तो वे पैटर्न मैचिंग का उपयोग करते हैं। इससे कोड की साफ और समझदारी बनी रहती है।

## कैसे करें?
जब हमें किसी स्ट्रिंग से कुछ अक्षर हटाने की आवश्यकता होती है, तो हम पैटर्न मैचिंग के साथ प्रिंट कोड का उपयोग कर सकते हैं। यहां हमारे पास एक स्ट्रिंग "Hello World!" है और हमें इससे वर्ण या स्पेस हटाने हैं। तो हम निम्नलिखित कोड का उपयोग करेंगे:

```elixir
string = "Hello World!"
IO.puts String.replace(string, "[aeiou]", "")
```
उपरोक्त कोड का आउटपुट "Hll Wrld!" होगा। इसमें "e, o, o" यह सभी छोटे वर्ण हैं जो हमने हटाना चाहा था।

## गहराई से जांच
कुछ नए पाठकों के लिए, पैटर्न मैचिंग कॉन्सेप्ट नया हो सकता है। लेकिन यह Elixir में बहुत उपयोगी और आसान है। Elixir का यूजर इंटरफ़ेस चरण पैटर्न मैचिंग का पूर्ण उदाहरण है। आप आधिकारिक Elixir डॉक्यूमेंटरी से और अन्य संसाधनों से और पैटर्न मैचिंग की गहराई समझ सकते हैं।

## देखें भी
- [पैटर्न मैचिंग कानून](https://elixir-lang.org/getting-started/pattern-matching.html)
- [पैटर्न मैचिंग ट्यूटोरियल](https://elixir-lang.org/getting-started/pattern-matching.html#working-with-patterns)
- [Elixir रीलेस नोट्स](https://github.com/elixir-lang/elixir/releases/tag/v1.11.2)