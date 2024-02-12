---
title:                "स्ट्रिंग को छोटे अक्षरों में परिवर्तित करना"
date:                  2024-01-20T17:38:26.012487-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग को छोटे अक्षरों में परिवर्तित करना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
जब हम किसी स्ट्रिंग को लोअर केस (छोटे अक्षरों में) बदलते हैं, तो इसका मतलब है की सभी अक्षरों को उनके संबंधित छोटे रूप में बदलना। प्रोग्रामर यह तब करते हैं जब उन्हें केस सेंसिटिविटी से निपटना पड़ता है, जैसे कि यूजर इनपुट का सामंजस्य करना या डेटा संग्रहणीयता को बढ़ाना।

## How to: (कैसे करें:)
Elixir में स्ट्रिंग को लोअर केस में कन्वर्ट करने के लिए `String.downcase/2` फ़ंक्शन का इस्तेमाल करते हैं। नीचे दिए गए कोड सैंपल और उत्पादन को देखिए:

```elixir
original_string = "नमस्ते, Elixir!"
lowercase_string = String.downcase(original_string)

IO.puts original_string
IO.puts lowercase_string
```

जब आप उपरोक्त कोड चलाएंगे, आपको यह उत्पादन मिलेगा:

```
नमस्ते, Elixir!
नमस्ते, elixir!
```

## Deep Dive (गहन जानकारी)
Elixir में `String.downcase/2` फ़ंक्शन Unicode text के साथ भी काम करता है, इसलिए यह विभिन्न भाषाओं और प्रतीकों को समर्थन करता है। इतिहास में जा कर देखें तो, यह फ़ंक्शन एलिक्सिर में जल्दी से उपलब्ध हो गया था क्योंकि टेक्स्ट प्रोसेसिंग एक कॉमन टास्क है। एल्टरनेटिव्स की बात करें तो, शायद ही कोई बेहतर विकल्प हैं क्योंकि Elixir का यह फ़ंक्शन अधिकांश जरूरतों के लिए पर्याप्त है। इम्प्लीमेंटेशन की बात करें तो, `String.downcase/2` हार्डवेयर के नीचे एर्लांग के Unicode मॉड्यूल को लेवरेज करता है ताकि परफॉरमेंस और एक्यूरेसी सुनिश्चित हो सके।

## See Also (इसे भी देखें)
अधिक जानकारी और स्ट्रिंग हैंडलिंग के बेस्ट प्रैक्टिस के लिए, Elixir की ऑफिशल डॉक्यूमेंटेशन और फोरम्स पर विजिट करें:

- [Elixir Documentation for String.downcase](https://hexdocs.pm/elixir/String.html#downcase/2)
- [Elixir Forum for discussions](https://elixirforum.com/)
- [Elixir School for learning more about strings](https://elixirschool.com/en/lessons/basics/strings/)
