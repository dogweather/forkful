---
title:                "नई परियोजना शुरू करना"
aliases:
- hi/elixir/starting-a-new-project.md
date:                  2024-01-20T18:03:59.211483-07:00
model:                 gpt-4-1106-preview
simple_title:         "नई परियोजना शुरू करना"

tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
नई प्रोजेक्ट शुरू करने का मतलब है एक ताज़ा आधार से कोडिंग करना। प्रोग्रामर्स इसे नई चीजों को सीखने, कस्टम समाधान बनाने और अपने आइडियाज़ को साकार करने के लिए करते हैं।

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
