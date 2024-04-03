---
date: 2024-01-20 17:36:56.818342-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) ."
lastmod: '2024-03-13T22:44:51.757317-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u0924\u093E\u0930\u0940\u0916 \u0915\u094B \u0938\u094D\u091F\u094D\u0930\
  \u093F\u0902\u0917 \u092E\u0947\u0902 \u092C\u0926\u0932\u0928\u093E"
weight: 28
---

## How to: (कैसे करें:)
```Elixir
# तारीख को स्ट्रिंग में बदलना

# Date module का उपयोग करके आज की तारीख प्राप्त करें
today = Date.utc_today()

# Date.to_string/1 function से तारीख को स्ट्रिंग में बदलें
date_string = Date.to_string(today)

# आउटपुट दिखाना
IO.puts(date_string)
```
सैंपल आउटपुट:
```
"2023-03-18"
```

## Deep Dive (गहराई से जानकारी):
एलिक्सिर में, `Date.to_string/1` डेटा को ISO 8601 सूचना मानक में बदलता है, जो कि एक अंतरराष्ट्रीय मानक है। यह कोड रिलीज 1.3 से मौजूद है। विकल्पों में `Timex` जैसे थर्ड-पार्टी पैकेज शामिल हैं, जिनसे और भी अनुकूलित फॉर्मेटिंग संभव है। अंतर्निहित कार्यान्वयन सीधी और प्रभावी है, लेकिन जटिल फॉर्मेटिंग के लिए आपको अतिरिक्त कोडिंग करनी पड़ सकती है।

## See Also (अन्य स्रोत):
- [Date.to_string/1 Documentation](https://hexdocs.pm/elixir/Date.html#to_string/1)
- [ISO 8601 Standard Information](https://www.iso.org/iso-8601-date-and-time-format.html)
- [Timex Elixir Package on Hex.pm](https://hex.pm/packages/timex)
