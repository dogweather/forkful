---
title:                "भविष्य या अतीत में तारीख की गणना"
date:                  2024-01-20T17:31:52.069582-07:00
model:                 gpt-4-1106-preview
simple_title:         "भविष्य या अतीत में तारीख की गणना"

category:             "Elixir"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elixir/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
भविष्य या अतीत में तारीखें गणना करने का मतलब है, किसी निर्धारित तारीख से कुछ दिन, महीने या सालों के बाद या पहले की तारीख ढूंढना। प्रोग्रामर्स यह काम अक्सर प्रोजेक्ट्स प्लानिंग, डेडलाइन्स सेट करने, या इतिहासिक डेटा एनालिसिस में करते हैं।

## कैसे करें? (How to:)
```elixir
# Elixir में DateTime मॉड्यूल का उपयोग करके तारीखों की गणना
# आइए मान लें कि आज की तारीख है और हमें आगे और पीछे की तारीख चाहिए

# Elixir का परिवेश सेटअप करें
alias DateTime, as: DT

# आज की तारीख
today = DT.utc_now()
IO.puts "आज की तारीख: #{today}"

# 10 दिन आगे
future_date = DT.add(today, 10 * 24 * 60 * 60)
IO.puts "10 दिन बाद की तारीख: #{future_date}"

# 10 दिन पिछली
past_date = DT.add(today, -10 * 24 * 60 * 60)
IO.puts "10 दिन पहले की तारीख: #{past_date}"
```
उपरोक्त कोड आपको बताएगा कि आज से 10 दिन पहले और 10 दिन बाद की तारीख क्या होगी।

## गहन जानकारी (Deep Dive)
प्रोग्रामिंग में तारीखों की गणना की उपयोगिता बहुत पहले से है। लोग दिनों की गिनती करने के लिए कैलेण्डर का उपयोग करते थे, और अब हम सॉफ्टवेयर में इस तरह के कैलकुलेशन्स स्वचालित रूप से कर सकते हैं। Elixir जैसी मॉडर्न प्रोग्रामिंग भाषाएँ `DateTime` मॉड्यूल प्रदान करती हैं। 

इसके अल्टर्नेट में `Timex` जैसे तीसरे पक्ष के लाइब्रेरी होते हैं जो अधिक समृद्ध फीचर्स प्रदान करती हैं। `DateTime.add/3` फंक्शन यूनिक्स इपॉक से सेकंड्स में घड़ी का समय जोड़ने या घटाने के लिए सेकंड्स का इस्तेमाल करता है, जो कि अत्यंत सुविधाजनक है।

## अतिरिक्त संसाधन (See Also)
- [Elixir `DateTime` डॉक्यूमेंटेशन](https://hexdocs.pm/elixir/DateTime.html)
- [Elixir School: Dates and Times](https://elixirschool.com/en/lessons/basics/date-time/)
- [Timex - Elixir डेट और टाइम लाइब्रेरी](https://github.com/bitwalker/timex)
