---
title:                "वर्तमान तिथि प्राप्त करना"
html_title:           "Lua: वर्तमान तिथि प्राप्त करना"
simple_title:         "वर्तमान तिथि प्राप्त करना"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/getting-the-current-date.md"
---

{{< edit_this_page >}}

## यह क्या है और क्यों?

"वर्तमान तारीख क्या है?" Lua में वर्तमान तारीख को प्राप्त करना अपने आप में एक उपयोगी कार्य है। कई समयों में, प्रोग्रामर्स अपने प्रोग्रामों को दिनांक, महीना या वर्ष के आधार पर विभिन्न कार्रवाई करने के लिए उपयोग करते हैं।

## कैसे करें:

```Lua
-- वर्तमान तारीख को प्राप्त करने के लिए लाइब्रेरी लोड करें
local date = require("date")

-- वर्तमान तारीख को प्राप्त करने के लिए इस्तेमाल किए गए तरीके का उदाहरण
local current_date = date()
print(current_date)

-- वर्तमान तारीख को जानने के लिए अलग-अलग प्रारूपों में आउटपुट प्राप्त करें
print(current_date:format("%a, %b %d, %Y")) -- उदाहरण आउटपुट: Fri, Jun 11, 2021
print(current_date:format("%x")) -- उदाहरण आउटपुट: 06/11/21
```

## गहराई में जाएं:

**ऐतिहासिक परिप्रेक्ष्य:** वर्तमान तारीख को प्राप्त करने की जरूरत और ये कार्य Lua में किसी को भी चौंकाने वाली बात नहीं है। यह Lua ने हमेशा से समय को व्यवस्थित करने के लिए एक सरल और प्रभावी तरीके को अपनाया है।

**विकल्प:** Lua के अलावा यदि आप अपने प्रोग्राम में वर्तमान तारीख को प्राप्त करने के इसी तरीके का उपयोग नहीं करना चाहते हैं, तो आप Date लाइब्रेरी का उपयोग कर सकते हैं।

**निष्पादन विवरण:** यदि आप किसी दूसरी भाषा का उपयोग कर रहे हैं जो Lua से अलग हो सकती है, तो आपको यह निष्पादन विवरण में थोड़ी बदलाव करने की आवश्यकता हो सकती है। इसलिए, आप अपने प्रोग्राम के लिए व्यापक जानकारी की जांच करें।

## इससे जुड़े लिंक:

- Lua मानक लाइब्रेरी: http://lua-users.org/wiki/GettingCurrentDate
- Date लाइब्रेरी: https://www.lua.org/pil/22.1.html