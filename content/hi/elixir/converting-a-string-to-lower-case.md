---
title:                "एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
html_title:           "Kotlin: एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
simple_title:         "एक स्ट्रिंग को लोअर केस में परिवर्तित करना"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

स्ट्रिंग को लोअर केस में बदलना यानी हर किरक्टर को छोटे अक्षर में बदलना। प्रोग्रामर्स इसे तब करते हैं जब वे डाटा को संवेदनशीलता से मुक्त करना चाहते हैं, उदाहरण के लिए, किसी इनपुट में छोटे और बड़े अक्षर का कोई अर्थ नहीं होता।

## कैसे करें:

Elixir में, हम `String.downcase/1` फ़ंक्शन का उपयोग करके string को निचले केस में बदल सकते हैं। निम्नलिखित कोड स्निपेट देखें:

```elixir
input = "नमस्ते, Elixir!"
IO.puts(String.downcase(input))
```

इसे चलाने से हमें निम्नलिखित आउटपुट मिलता है:

```elixir
"नमस्ते, elixir!"
```

## गहरा अध्ययन:

Elixir में `String.downcase/1` का परिचय 1.0.0 संस्करण में हुआ। यह फ़ंक्शन यूनिकोड आधारित है, इसलिए यह किसी भी भाषा के अक्षरों को सही दृष्टिकोण से संभाल सकता है।

विकल्प के रूप में, यदि आप केवल ASCII किरकटर के साथ काम कर रहे हैं, तो आप `:string.downcase/1` फ़ंक्शन का उपयोग कर सकते हैं, जो एर्लैंग में निर्दिष्ट होता है। लेकिन अधिकतर मामलों में, Elixir's `String.downcase/1` ज्यादा कारगर है।

## देखें भी:

- Elixir डॉक्स की `String` मॉड्यूल: [link](https://hexdocs.pm/elixir/String.html).
- Erlang `:string` मॉड्यूल: [link](https://erlang.org/doc/man/string.html).
- यूनिकोड और Elixir: [link](https://elixir-lang.org/blog/2017/01/05/unicode-and-elixir/).