---
title:                "वर्तमान तारीख प्राप्त करना"
html_title:           "C#: वर्तमान तारीख प्राप्त करना"
simple_title:         "वर्तमान तारीख प्राप्त करना"
programming_language: "Elixir"
category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/getting-the-current-date.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
एलिक्सिर में वर्तमान दिनांक प्राप्त करना कुछ इस प्रकार का कार्य होता हैं जिसमें हम सिस्टम के कैलेंडर से वर्तमान दिनांक की सूचना प्राप्त करते हैं। प्रोग्रामरों को इसे कामयाबी से निभाने की आवश्यकता होती हैं क्योंकि यह कार्य तारीख-संबंधी लॉग, समय-संबंधी सूचना, और टाइम ट्रैकिंग के सेट आपरेशनों में महत्वपूर्ण भूमिका निभाता हैं। 

## कैसे?
एलिक्सिर में वर्तमान दिनांक को पाने के लिए, आप निम्नलिखित code का उपयोग कर सकते हैं:

```Elixir
date = Date.utc_today()
IO.inspect(date)
```
जोड़ने वाली पंक्तियों का उपयोग करके कार्यान्वयन करने पर तुम्हें निम्नलिखित परिणाम मिलेगा:

```Elixir
~D[2022-04-05]
```

## तीव्र गोति
प्रत्येक प्रोग्रामिंग भाषा में वर्तमान दिनांक को पाने के विभिन्न तरीके होते हैं, और एलिक्सिर इसमें कोई अपवाद नहीं हैं। एलिक्सिर में, आप `Date` मॉड्यूल का उपऔग कर सकते हैं जो `fatal` और `non-fatal` यानि `!` and `non-!` वेरिएंट उपलब्ध कराती हैं। 

## अन्य स्रोत
[Elixir get current date](https://elixirforum.com/t/getting-the-current-date-and-time/1963/6)
[Official Elixir Language Date Documentation](https://hexdocs.pm/elixir/Date.html)