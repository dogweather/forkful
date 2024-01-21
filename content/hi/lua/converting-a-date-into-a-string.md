---
title:                "तारीख को स्ट्रिंग में बदलना"
date:                  2024-01-20T17:37:18.116169-07:00
model:                 gpt-4-1106-preview
simple_title:         "तारीख को स्ट्रिंग में बदलना"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
डेट को स्ट्रिंग में बदलने का मतलब है आसानी से पढ़ने योग्य फॉर्म में तारीख का प्रदर्शन करना। प्रोग्रामर्स यह इसलिए करते हैं ताकि तारीखों को यूज़र इंटरफ़ेस पर दिखाया जा सके या लॉग फ़ाइलों में सेव किया जा सके।

## How to: (कैसे करें:)
```Lua
os.date("%Y-%m-%d")
```
यह कोड आज की तारीख को YYYY-MM-DD फॉर्मेट में दिखाएगा।

```Lua
local t = os.time() -- वर्तमान टाइमस्टैम्प
local formattedDate = os.date("%B %d, %Y", t)
print(formattedDate)
```
उदाहरण आउटपुट: `March 18, 2023`

## Deep Dive (गहन अध्ययन)
Lua में `os.date` फंक्शन तारीख को स्ट्रिंग में बदलता है और इसे एक विशिष्ट फॉर्मेट में प्रदर्शित करने की अनुमति देता है। यह सी लाइब्रेरी के `strftime` जैसा काम करता है। जबकि `os.date` बहुत लचीला है, अन्य भाषाओं में वैकल्पिक रूप से मौजूद डेट प्रोसेसिंग लाइब्रेरीज जैसे कि `DateTime` या `moment.js` Lua में नहीं हैं।

हिस्टोरिकल कॉन्टेक्स की बात करें तो, Lua का पहला वर्जन 1993 में आया था, और तब से समय और तारीख से जुड़े कामों को एक बुनियादी सेट के तौर पर हैंडल किया गया। अगर आपको ज्यादा जटिल तारीख ऑपरेशंस की जरूरत हो, तो आप `luadate` जैसे किसी थर्ड-पार्टी मॉड्यूल पर निर्भर हो सकते हैं।

## See Also (और भी देखें)
- Lua मैनुअल, `os.date` फंक्शन के लिए: [Lua 5.4 Reference Manual](https://www.lua.org/manual/5.4/manual.html#pdf-os.date)
- `luadate` लाइब्रेरी, विस्तारित डेट फंक्शन्स के लिए: [luadate on GitHub](https://github.com/Tieske/date)
- तारीखों के साथ काम करना, Lua प्रोग्रामिंग ज्ञानकोष: [Lua Users Wiki: Dates and Time](http://lua-users.org/wiki/DatesAndTime)