---
title:                "रेगुलर एक्सप्रेशन का उपयोग"
html_title:           "Bash: रेगुलर एक्सप्रेशन का उपयोग"
simple_title:         "रेगुलर एक्सप्रेशन का उपयोग"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
रेगुलर एक्सप्रेशन्स (Regular Expressions) पैटर्न मैचिंग के लिए उपयोग किए जाते हैं। ये प्रोग्रामर्स को टेक्स्ट सर्च, डेटा वैलिडेशन, और क्लीनिंग में सहायता करते हैं।

## How to: (कैसे करें:)
```elixir
# टेक्स्ट में 'Elixir' शब्द की खोज
regex = ~r/Elixir/
String.match?("I love programming in Elixir!", regex)
# आउटपुट: true

# पैटर्न के साथ कैप्चर ग्रुप बनाना
regex_captures = ~r/(\d{4})-(\d{2})-(\d{2})/
String.scan("2023-04-12", regex_captures)
# आउटपुट: [["2023", "04", "12"]]
```

## Deep Dive (गहन जानकारी)
रेगुलर एक्सप्रेशन्स की उत्पत्ति सैद्धांतिक कंप्यूटर विज्ञान और औपचारिक भाषा सिद्धांत से हुई है। एलिक्सर में `Regex` मॉड्यूल इनका प्रयोग करता है। एल्टरनेटिव्स में स्ट्रिंग पैटर्न मैचिंग के अन्य तरीके शामिल हैं, जैसे कि `String.contains?` या स्ट्रिंग फ़ंक्शन्स के अन्य ऑप्शन्स। `Regex` मॉड्यूल अंडरलाइंग ERTS (Erlang Runtime System) का प्रयोग करता है।

## See Also (और जानकारी के लिए)
- [Elixir Regex Module](https://hexdocs.pm/elixir/Regex.html)
- [Erlang's documentation on regex](https://erlang.org/doc/man/re.html)
