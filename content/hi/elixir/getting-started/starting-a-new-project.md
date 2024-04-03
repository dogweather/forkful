---
date: 2024-01-20 18:03:59.211483-07:00
description: "How to (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902): Elixir \u092E\
  \u0947\u0902 \u0928\u092F\u093E \u092A\u094D\u0930\u094B\u091C\u0947\u0915\u094D\
  \u091F Mix \u091F\u0942\u0932 \u0915\u093E \u0907\u0938\u094D\u0924\u0947\u092E\u093E\
  \u0932 \u0915\u0930 \u0915\u0947 \u0936\u0941\u0930\u0942 \u0915\u093F\u092F\u093E\
  \ \u091C\u093E\u0924\u093E \u0939\u0948."
lastmod: '2024-03-13T22:44:51.737852-06:00'
model: gpt-4-1106-preview
summary: "Elixir \u092E\u0947\u0902 \u0928\u092F\u093E \u092A\u094D\u0930\u094B\u091C\
  \u0947\u0915\u094D\u091F Mix \u091F\u0942\u0932 \u0915\u093E \u0907\u0938\u094D\u0924\
  \u0947\u092E\u093E\u0932 \u0915\u0930 \u0915\u0947 \u0936\u0941\u0930\u0942 \u0915\
  \u093F\u092F\u093E \u091C\u093E\u0924\u093E \u0939\u0948."
title: "\u0928\u0908 \u092A\u0930\u093F\u092F\u094B\u091C\u0928\u093E \u0936\u0941\
  \u0930\u0942 \u0915\u0930\u0928\u093E"
weight: 1
---

## How to (कैसे करें):
Elixir में नया प्रोजेक्ट Mix टूल का इस्तेमाल कर के शुरू किया जाता है:

```elixir
mix new greet_everyone
```

आउटपुट:

```plaintext
* creating README.md
* creating .formatter.exs
* creating .gitignore
* creating mix.exs
* creating lib
* creating lib/greet_everyone.ex
* creating test
* creating test/test_helper.exs
* creating test/greet_everyone_test.exs

Your Mix project was created successfully.
You can use "mix compile" to compile it,
"use 'mix test' to test it,
or 'iex -S mix' for an interactive prompt.
```

यह कमांड आपके लिए एक नई Elixir एप्लीकेशन की संरचना बना देता है।

```elixir
cd greet_everyone
iex -S mix
```

यह कमांड आपको इंटरएक्टिव प्रॉम्प्ट में ले जाती है, जहाँ आप अपने कोड को लाइव टेस्ट कर सकते हैं।

## Deep Dive (गहराई में):
Elixir में Mix एक बिल्ड टूल है जो कि 2012 में José Valim ने बनाया। यह आधुनिक प्रोजेक्ट्स के लिए एक स्कैफोल्डिंग टूल की तरह काम करता है। Mix के विकल्प में rebar3 है, जिसका इस्तेमाल Erlang प्रोजेक्ट्स के लिए होता है। Mix आपको डिपेंडेन्सी मैनेजमेंट, कॉन्फिगरेशन, टेस्टिंग और कई दूसरे टास्क्स आसानी से करने देता है। प्रोजेक्ट शुरू करना और प्रोजेक्ट का मैनेजमेंट बहुत महत्वपूर्ण है, इसीलिए Mix जैसे टूल्स डेवलपर्स की बहुत मदद करते हैं।

## See Also (और भी देखें):
- Elixir की आधिकारिक वेबसाइट: [Elixir Lang](https://elixir-lang.org/)
- Mix डॉक्युमेंटेशन: [Mix Task](https://hexdocs.pm/mix/Mix.html)
- Elixir स्कूल: [Elixir School](https://elixirschool.com/en/)

इन लिंक्स पर जाकर आप Elixir के बारे में और भी जानकारी प्राप्त कर सकते हैं।
