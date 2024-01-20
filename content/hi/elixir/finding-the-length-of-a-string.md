---
title:                "स्ट्रिंग की लंबाई पता करना"
html_title:           "C++: स्ट्रिंग की लंबाई पता करना"
simple_title:         "स्ट्रिंग की लंबाई पता करना"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

स्ट्रिंग की लंबाई का पता लगाना मतलब उसमें जो करैक्टर्स हैं उनकी संख्या का पता करना। यह जानना महत्वपूर्ण हो सकता है - उदाहरण स्वरूप, जब हमें टेक्स्ट को सामग्री को एक विषेष आकर में ठीक करने की आवश्यकता हो।

## कैसे करे:

Elixir में, हम `String.length/1` का उपयोग करके एक स्ट्रिंग की लंबाई पा सकते हैं। यहां एक उदाहरण है:

```Elixir
str = "नमस्ते, दुनिया"
IO.puts String.length(str)
```

इस मामले में, यह आउटपुट `13` होगा, क्योंकि उस स्ट्रिंग में 13 करैक्टर हैं।

## गहरा अध्ययन:

ऐलिक्सर में `String.length/1` फ़ंक्शन काम करने के लिए यूनिकोड और बाइनरी एन्कोडिंग का उपयोग करता है। इसके विपरीत, कुछ अन्य भाषाओं में स्ट्रिंग की लंबाई के लिए जीवन में घटित होने वाले किरदारों की संख्या पर आधारित होती है।

ऐलिक्सर में, हम एर्नियान फ़ंक्शन `byte_size/1` का भी उपयोग कर सकते हैं, लेकिन यह यूनिकोड करैक्टर्स को सही रूप से हैंडल नहीं करता है, इसलिए हमेशा `String.length/1` का उपयोग करना बेहतर होगा।

## भी देखें:

1. [Elixir documentation for `String.length/1`](https://hexdocs.pm/elixir/String.html#length/1)
2. [Erlang `byte_size/1` documentation](http://erlang.org/doc/man/erlang.html#byte_size-1)
3. [Elixir School String Lesson](https://elixirschool.com/en/lessons/basics/strings/)