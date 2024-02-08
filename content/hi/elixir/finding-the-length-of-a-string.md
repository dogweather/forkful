---
title:                "स्ट्रिंग की लंबाई ज्ञात करना"
aliases:
- hi/elixir/finding-the-length-of-a-string.md
date:                  2024-01-20T17:47:52.502347-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग की लंबाई ज्ञात करना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
String की लंबाई (length) पता लगाना मतलब है जानना कि string में कितने characters हैं। Programmers इसलिए लंबाई नापते हैं, ताकि वे data validation, UI display या फिर text processing को सही से कर सकें।

## How to: (कैसे करें:)
Elixir में String की लंबाई जानने के लिए `String.length/1` फंक्शन का इस्तेमाल करें।

```elixir
# String की लंबाई पता करना
length = String.length("नमस्ते")
IO.puts(length)  # यह 6 प्रिंट करेगा क्योंकि "नमस्ते" में 6 characters हैं।
```

यदि आप UTF-8 में स्टोर किए गए binary फॉर्मैट में strings के बारे में सोच रहे हैं, तो यह ध्यान में रखें कि Elixir में इसकी लंबाई के लिए बाइट्स की संख्या अलग हो सकती है।

```elixir
# Binary की लंबाई पता करना
byte_size = byte_size("नमस्ते")
IO.puts(byte_size)  # यह 18 प्रिंट करेगा क्योंकि "नमस्ते" UTF-8 में 18 bytes का है।
```

## Deep Dive (गहन जानकारी)
String की लंबाई जानना एक सरल कार्य लगता है, पर UTF-8 एनकोडेड strings के साथ, हर character एक से अधिक byte का हो सकता है। इस कारण `String.length/1` और `byte_size/1` के परिणाम अलग होते हैं। पुराने प्रोग्रामिंग भाषाओं में, जहां हर character ठीक एक byte का होता था, यह काम आसान था।

Elixir इसे इफिशिएंट बनाती है और यूजर के लिए बहुत सारे विकल्प प्रदान करती है, जैसे कि pattern matching और recursive functions के जरिए string पर काम करना। इसका `String` मॉड्यूल robust है और बहुत सारे functions ऑफर करता है जो string manipulation को आसान बनाते हैं।

## See Also (और भी जानकारी)
- Elixir के आधिकारिक डॉक्युमेंटेशन में `String` मॉड्यूल: [Elixir String](https://hexdocs.pm/elixir/String.html)
- यूनिकोड और UTF-8 के बीच अंतर को समझने के लिए: [Unicode Strings and UTF-8](https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html#unicode-and-utf-8)
- String manipulation के लिए Elixir पर प्रैक्टिकल एक्साम्पल्स देखने के लिए: [Elixir School: Strings](https://elixirschool.com/en/lessons/basics/strings/)
