---
title:                "स्ट्रिंग को जोड़ना"
date:                  2024-01-20T17:34:36.363236-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग को जोड़ना"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
String को जोड़ना इसका मतलब है कि दो या उससे ज्यादा texts को एक साथ करना। Programmers यह इसलिए करते हैं ताकि dynamic मैसेज बना सकें, data को format कर सकें या फिर output को customized कर सकें।

## How to: (कैसे करें:)
Elixir में strings को जोड़ना आसान है। यहाँ कुछ तरीके दिए गए हैं:

```elixir
# String Interpolation (स्ट्रिंग इंटरपोलेशन)
name = "रोहित"
message = "नमस्ते, #{name}"
IO.puts message  # इसका आउटपुट: नमस्ते, रोहित

# Concatenation using `<>` operator (<> ऑपरेटर का उपयोग करके जोड़ना)
greeting = "नमस्ते " <> "दुनिया"
IO.puts greeting  # इसका आउटपुट: नमस्ते दुनिया
```

## Deep Dive (गहराई में जानकारी)
String concatenation की history उतनी ही पुरानी है जितनी programming की। पहले के computers में memory कम होने के कारण, efficient string handling महत्वपूर्ण थी। Elixir में, string concatenation को optimize किया गया है क्योंकि Elixir का string data UTF-8 encoded होता है, जिससे multibyte characters (जैसे हिंदी अक्षर) को संभालना आसान होता है।

Alternatives में string lists का उपयोग शामिल है जिसे I/O lists कहा जाता है, जिन्हें efficiently output या फिर large strings के साथ work करते समय उपयोग किया जा सकता है।

```elixir
# I/O Lists
hello = "नमस्ते"
world = "दुनिया"
io_list = [hello, " ", world]
IO.puts(io_list)  # इसका आउटपुट वही होगा: नमस्ते दुनिया
```

यह concat करने के लिए मेमोरी को efficient तरीके से उपयोग करता है, क्योंकि इसमें आपको हर बार पूरे string की copy नहीं बनानी पड़ती।

## See Also (और जानकारी के लिए)
- Elixir के official documentation में strings के बारे में और जानने के लिए: [Elixir Strings](https://elixir-lang.org/getting-started/binaries-strings-and-char-lists.html)
- Elixir Forum पर string concatenation के विषय में चर्चा: [Elixir Forum: String Concatenation](https://elixirforum.com)
- Elixir School में Elixir की basics सीखने के लिए: [Elixir School: Basics](https://elixirschool.com/en/lessons/basics/strings)