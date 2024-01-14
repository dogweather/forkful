---
title:                "Elixir: स्ट्रिंग की लंबाई को खोजना"
simple_title:         "स्ट्रिंग की लंबाई को खोजना"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

# क्यों

एलिक्सिर में स्ट्रिंग की लंबाई को जानने का क्या महत्त्व है? स्ट्रिंग लंबाई को जानने से हम अपनी कोडिंग को और आसान बना सकते हैं और अपने प्रोग्राम में स्ट्रिंग को परिवर्तित कर सकते हैं।

# कैसे करें

एलिक्सिर में स्ट्रिंग की लंबाई को पता करने के लिए हम एक built-in फंक्शन `String.length/1` का उपयोग कर सकते हैं। इसके अलावा हम लोग अपने फंक्शन में भी स्ट्रिंग की लंबाई को पता कर सकते हैं। यहां हम एक उदाहरण देखेंगे:

```elixir
string = "मेरा नाम मोहित है"
IO.puts String.length(string)
# आउटपुट: 16
```

# गहराई में जाएं

स्ट्रिंग की लंबाई को जानने के लिए हम एक `count` वेल्यू को इंक्रीमेंट करते हैं जब तक कि वह स्ट्रिंग के अंत तक नहीं पहुंचता है। इसके अलावा हम अन्य तरीकों से भी स्ट्रिंग की लंबाई को पता कर सकते हैं। यह एक महत्वपूर्ण कौशल है जो हर एलिक्सिर डेवलपर को जानना चाहिए।

# देखें भी

- [एलिक्सिर ऑफिशियल डॉक्युमेंटेशन](https://hexdocs.pm/elixir/String.html#length/1)
- [Learn Elixir in Y minutes - स्ट्रिंग की लंबाई को पता करना](https://learnxinyminutes.com/docs/elixir-hi/#string-length)
- [Elixir School - स्ट्रिंग](https://elixirschool.com/hi/lessons/basics/string/)