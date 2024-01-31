---
title:                "दो तारीखों की तुलना"
date:                  2024-01-20T17:33:18.058794-07:00
model:                 gpt-4-1106-preview
simple_title:         "दो तारीखों की तुलना"

category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
तारीखों की तुलना करना मतलब है पता करना कि एक तारीख दूसरे से पहले है, बाद में है या बराबर है। प्रोग्रामर्स इसका उपयोग समय सम्बंधित फीचर्स, जैसे की deadline checks और time-based sorting के लिए करते हैं।

## How to: (कैसे करें:)
Elixir में दो तारीखें तुलना करने के लिए `Date` मॉड्यूल का उपयोग होता है।

```elixir
# तारीख बनाना
date1 = ~D[2023-03-01]
date2 = ~D[2023-04-01]

# तारीखों की तुलना
before = Date.compare(date1, date2) # :lt (less than)
same = Date.compare(date1, date1)   # :eq (equal)
after = Date.compare(date2, date1)  # :gt (greater than)

IO.puts "date1 is before date2: #{before}"
IO.puts "date1 is same as date1: #{same}"
IO.puts "date2 is after date1: #{after}"
```

सैंपल आउटपुट:

```
date1 is before date2: lt
date1 is same as date1: eq
date2 is after date1: gt
```

## Deep Dive (गहन अंतर्दृष्टि)
Elixir में `Date.compare/2` फंक्शन दिनांक तुलना के लिए एक मानक तरीका है, जो कि 2016 में Elixir 1.3 के रिलीज के साथ आया। Elixir, Erlang वर्चुअल मशीन पर चलता है और प्राकृतिक रूप से concurrent programming से मेल खाता है। तारीखों की तुलना UTF-8 कैलेंडर स्टैंडर्ड्स के अनुसार होती है।

वैकल्पिक लाइब्रेरीज जैसे कि Timex भी मौजूद हैं, जो अतिरिक्त फंक्शनलिटीज प्रदान करती हैं, पर बुनियादी तारीख तुलना के लिए Elixir का मानक लाइब्रेरी पर्याप्त है। इसका कार्यान्वयन Elixir के कोड में highly optimized है, जिससे प्रदर्शन में लाभ होता है।

## See Also (और जानकारी के लिए)
- Elixir का ऑफिशियल डॉक्यूमेंटेशन: [Date](https://hexdocs.pm/elixir/Date.html)
- Timex लाइब्रेरी: [Timex on Hex.pm](https://hex.pm/packages/timex)
- Elixir फोरम, डिस्कशन के लिए: [Elixir Forum](https://elixirforum.com/)
- Learn Elixir, एक फ्री पाठ्य सामग्री : [Learn Elixir](https://elixirschool.com/en/)
