---
title:                "टेक्स्ट फाइल लिखना"
html_title:           "Bash: टेक्स्ट फाइल लिखना"
simple_title:         "टेक्स्ट फाइल लिखना"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
टेक्स्ट फ़ाइल लिखना डेटा को सहेजना है। प्रोग्रामर इसे लॉग्स, कॉन्फ़िगरेशन और डेटा एक्सचेंज के लिए करते हैं।

## How to: (कैसे करें:)
```elixir
# फ़ाइल खोलना और लिखना
File.write!("hello.txt", "नमस्ते दुनिया!")

# पढ़कर जाँच करना कि सही लिखा गया है
IO.puts(File.read!("hello.txt"))
```
आउटपुट:
```
नमस्ते दुनिया!
```

## Deep Dive (गहराई में जानकारी)
टेक्स्ट फ़ाइलें लिखना प्रोग्रामिंग की शुरुआत से डेटा स्टोर करने का एक आसान तरीका रहा है। Elixir में `File` मॉड्यूल इस काम के लिए उपयोग होता है। विकल्पों में डेटाबेस और बाइनरी फ़ाइलें शामिल हैं, पर टेक्स्ट फ़ाइलें सरलता और पठनीयता के लिए अच्छी होती हैं।

## See Also (और जानकारी के लिए)
- [Elixir File module documentation](https://hexdocs.pm/elixir/File.html)
- [Programming Elixir by Dave Thomas](https://pragprog.com/titles/elixir16/programming-elixir-1-6/)
- [Elixir School](https://elixirschool.com/en/)
