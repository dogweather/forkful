---
date: 2024-01-20 17:47:52.502347-07:00
description: "String \u0915\u0940 \u0932\u0902\u092C\u093E\u0908 (length) \u092A\u0924\
  \u093E \u0932\u0917\u093E\u0928\u093E \u092E\u0924\u0932\u092C \u0939\u0948 \u091C\
  \u093E\u0928\u0928\u093E \u0915\u093F string \u092E\u0947\u0902 \u0915\u093F\u0924\
  \u0928\u0947 characters \u0939\u0948\u0902\u0964 Programmers \u0907\u0938\u0932\u093F\
  \u090F \u0932\u0902\u092C\u093E\u0908 \u0928\u093E\u092A\u0924\u0947 \u0939\u0948\
  \u0902, \u0924\u093E\u0915\u093F \u0935\u0947 data validation, UI\u2026"
lastmod: '2024-03-11T00:14:25.589738-06:00'
model: gpt-4-1106-preview
summary: "String \u0915\u0940 \u0932\u0902\u092C\u093E\u0908 (length) \u092A\u0924\
  \u093E \u0932\u0917\u093E\u0928\u093E \u092E\u0924\u0932\u092C \u0939\u0948 \u091C\
  \u093E\u0928\u0928\u093E \u0915\u093F string \u092E\u0947\u0902 \u0915\u093F\u0924\
  \u0928\u0947 characters \u0939\u0948\u0902\u0964 Programmers \u0907\u0938\u0932\u093F\
  \u090F \u0932\u0902\u092C\u093E\u0908 \u0928\u093E\u092A\u0924\u0947 \u0939\u0948\
  \u0902, \u0924\u093E\u0915\u093F \u0935\u0947 data validation, UI\u2026"
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u0940 \u0932\u0902\
  \u092C\u093E\u0908 \u091C\u094D\u091E\u093E\u0924 \u0915\u0930\u0928\u093E"
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
