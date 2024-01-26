---
title:                "तारीख को स्ट्रिंग में बदलना"
date:                  2024-01-20T17:36:56.818342-07:00
model:                 gpt-4-1106-preview
simple_title:         "तारीख को स्ट्रिंग में बदलना"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

तारीख को स्ट्रिंग में बदलना यानि की डेटा फॉर्मेट को लिखित रूप में परिवर्तित करना। प्रोग्रामर इसे इसलिए करते हैं ताकि तारीखों को आसानी से पढ़ा और संग्रहित किया जा सके।

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
