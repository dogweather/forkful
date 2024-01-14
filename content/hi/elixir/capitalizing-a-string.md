---
title:                "Elixir: स्ट्रिंग को मालिकाना करना"
programming_language: "Elixir"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## क्यों

कोई भी व्यक्ति एक स्ट्रिंग को कैपिटलाइज करने में रुचि रख सकता है क्योंकि यह एक अक्षरिक रूप से इंपोर्टेंट ओपरेशन होता है जो प्रोग्रामिंग में उपयोगी हो सकता है।

## कैसे करें

```elixir
string = "hello world!"
capitalized_string = String.capitalize(string)
IO.puts capitalized_string
```

उपरोक्त उदाहरण में, हमने "hello world!" स्ट्रिंग को कैपिटलाइज किया है और उसे "Hello world!" में परिवर्तित किया है।

## गहराई में जाएं

स्ट्रिंग को कैपिटलाइज करने के लिए, String.capitalize/1 फंक्शन का उपयोग किया जाता है जो दिए गए स्ट्रिंग में से पहले अक्षर को कैपिटल बनाएगा। इसके अलावा, आपको String.upcase/1 फंक्शन भी उपयोग कर सकते हैं जो दिए गए स्ट्रिंग को पूरी तरह से अपरकेस में बदल देगा। अतिरिक्त गहराई के लिए, आप इस लेख को देख सकते हैं: [Elixir डॉक्स: String Module](https://hexdocs.pm/elixir/String.html)

## देखें भी

* [Elixir डॉक्स: String Module](https://hexdocs.pm/elixir/String.html)
* [Elixir डॉक्स: String.capitalize/1 फंक्शन](https://hexdocs.pm/elixir/String.html#capitalize/1)
* [Elixir डॉक्स: String.upcase/1 फंक्शन](https://hexdocs.pm/elixir/String.html#upcase/1)