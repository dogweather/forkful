---
title:                "Elixir: सन को निकालना"
simple_title:         "सन को निकालना"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/extracting-substrings.md"
---

{{< edit_this_page >}}

## इलिक्सिर प्रोग्रामिंग के लिए सुलभ समस्याओं का समाधान: आवश्यकता

इलिक्सिर प्रोग्रामिंग भाषा में substrings को प्राप्त करना एक सामान्य समस्या है जो निरंतर पैटर्न पहचान और प्रक्रिया तकनीकी अवधारणाओं का उपयोग करके हल किया जा सकता है। substrings को प्राप्त करने के लिए इस्तेमाल किए जाने वाले तकनीक आपके कोड को अधिक उन्नत और विस्तृत कर सकते हैं।

## कैसे करें substrings को प्राप्त

#### उदाहरण 1:
```elixir
string = "इलिक्सिर प्रोग्रामिंग भाषा"
substring = String.slice(string, 8..15)
IO.puts(substring)
```
उपयोगकर्ता की गई इनपुट: "इलिक्सिर प्रोग्रामिंग भाषा"
उत्पाद: "प्रोग्रामिंग"

#### उदाहरण 2:
```elixir
string = "यह केवल एक उदाहरण है"
substring = String.slice(string, 0..7)
IO.puts(substring)
```
उपयोगकर्ता की गई इनपुट: "यह केवल एक उदाहरण है"
उत्पाद: "यह केवल"

## गहराई तक substrings की जानकारी

सबसे पहले, substrings को समझने के लिए आपको पहले एक मूल शब्द में से उन वर्णों को चुनना पड़ता है जिनसे आपको उपलब्ध substrings को बनाना है। यह शब्द आपके `string` वेरिएबल में स्थानान्तरित हो जाते हैं और फिर इससे प्राप्त किए गए `substring` आपकी इच्छानुसार कम्प्यूट किया जाता है।

आप भी दो वर्ण रेंज से उत्पन्न होने वाले उदाहरण को इस तरह से समझ सकते हैं:
```elixir
string = "इलिक्सिर प्रोग्रामिंग भाषा"
substring = String.slice(string, 17..-4)
IO.puts(substring)
```
उपयोगकर्ता की ग