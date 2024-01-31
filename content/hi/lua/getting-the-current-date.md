---
title:                "वर्तमान तारीख प्राप्त करना"
date:                  2024-01-20T15:15:54.830781-07:00
html_title:           "C: वर्तमान तारीख प्राप्त करना"
simple_title:         "वर्तमान तारीख प्राप्त करना"

tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/getting-the-current-date.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
(1) 'वर्तमान तारीख' पाने का मतलब है सिस्टम की तिथि और समय को लेना। (2) प्रोग्रामर लॉगिंग, टाइमस्टैम्प्स, और समय-संबंधित फेचर के लिए यह करते हैं।

## How to: (कैसे करें:)
Lua में वर्तमान तारीख हासिल करने का आसान तरीका:

```Lua
local current_date = os.date("*t")  -- वर्तमान तारीख और समय का टेबल
print("आज की तारीख है:", current_date.day, current_date.month, current_date.year)
```

आउटपुट:
```
आज की तारीख है: 9 4 2023
```

## Deep Dive (गहराई से जानकारी)
Lua में os.date फंक्शन 1970 के यूनिक्स टाइम स्टैंडर्ड पर आधारित है। इसके दो प्रारूप हैं: एक स्ट्रिंग देता है और दूसरा टाइम टेबल। वैकल्पिक लाइब्रेरीज जैसे LuaDate भी हैं जो अधिक फंक्शन्स देती हैं। Lua में समय का प्रबंधन सरल है, लेकिन समय क्षेत्रों और ग्रीष्मकालीन समय की बचत जैसी जटिलताएँ थर्ड-पार्टी मॉड्यूल का उपयोग करने पर विचार की जा सकती हैं।

## See Also (और भी देखें)
- Lua ऑफिसियल डॉक्युमेंटेशन: [www.lua.org/manual/5.4/](http://www.lua.org/manual/5.4/)
- LuaDate: [github.com/Tieske/date](https://github.com/Tieske/date)
- Lua के लिए समय क्षेत्र मॉड्यूल: [luatz](https://github.com/daurnimator/luatz)
