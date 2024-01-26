---
title:                "स्ट्रिंग से दिनांक पार्स करना"
date:                  2024-01-20T15:38:32.816392-07:00
html_title:           "Arduino: स्ट्रिंग से दिनांक पार्स करना"
simple_title:         "स्ट्रिंग से दिनांक पार्स करना"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
तारीख को स्ट्रिंग से पार्स करना मतलब स्ट्रिंग के फॉर्मेट में दी गई तारीख को प्रोग्राम के समझने योग्य ढंग में बदलना है। कार्यक्रमकर्ता इसे तारीखों की गणना, तुलना और मान्यतानुसार फॉर्मैट करने के लिए करते हैं।

## कैसे करें:
```Lua
-- आइए समझें कि Lua में स्ट्रिंग से तारीख कैसे पार्स की जाती है।

-- एक स्ट्रिंग तारीख उदाहरण
local dateString = "दिनांक: 21-03-2023"

-- пат्टेर्न मिलान से तारीख प्राप्त करें
local pattern = "दिनांक: (%d+)-(%d+)-(%d+)"
local day, month, year = dateString:match(pattern)

-- तारीख को दिखाएं
print("दिन:", day, "महीना:", month, "वर्ष:", year)

-- os.time का उपयोग करते हुए तारीख में बदलें
local dateTable = { day = day, month = month, year = year }
local timestamp = os.time(dateTable)

-- टाइमस्टैम्प दिखाएं
print("Timestamp:", timestamp)
```

सैम्पल आउटपुट:
```
दिन: 21 महीना: 03 वर्ष: 2023
Timestamp: 1679327400
```

## गहराई से जानकारी:
तारीख को स्ट्रिंग से पार्स करना तब से हो रहा है जबसे कंप्यूटर्स और प्रोग्राम्स में तारीखों को समझने की जरूरत पड़ी। Lua में इसे साधारण `match` फंक्शन से किया जा सकता है, जो पैटर्न मिलान पर आधारित होता है। इसके अलावा, `os.date` और `os.time` जैसी बिल्ट-इन फंक्शन्स का उपयोग परिणामी डेटा को मानक Unix टाइमस्टैम्प में बदलने में मदद करता है। अल्टरनेटिव लाइब्रेरीज़ जैसे `date`, `Time` भी हैं, जो जटिल डेट ऑपरेशन्‍स के लिए अधिक सुविधाएं देते हैं।

## संबंधित स्रोत:
- Lua मैनुअल: http://www.lua.org/manual/
- एक `date` लाइब्रेरी का GitHub पेज: https://github.com/Tieske/date
- पैटर्न मिलान पर एक पोस्ट: http://lua-users.org/wiki/PatternsTutorial
- Lua में टाइमस्टैम्प और डेट्स के बारे में: https://www.lua.org/pil/22.1.html
