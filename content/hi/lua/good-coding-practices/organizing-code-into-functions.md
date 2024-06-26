---
date: 2024-01-26 01:17:12.617039-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: \u092B\u0902\u0915\
  \u094D\u0936\u0928 \u091C\u091F\u093F\u0932 \u0939\u094B \u0938\u0915\u0924\u0947\
  \ \u0939\u0948\u0902, \u0935\u093F\u092D\u093F\u0928\u094D\u0928 \u0915\u093E\u0930\
  \u094D\u092F\u094B\u0902 \u0915\u093E \u0938\u0902\u091A\u093E\u0932\u0928 \u0915\
  \u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902."
lastmod: '2024-04-05T21:53:54.536631-06:00'
model: gpt-4-0125-preview
summary: "\u092B\u0902\u0915\u094D\u0936\u0928 \u091C\u091F\u093F\u0932 \u0939\u094B\
  \ \u0938\u0915\u0924\u0947 \u0939\u0948\u0902, \u0935\u093F\u092D\u093F\u0928\u094D\
  \u0928 \u0915\u093E\u0930\u094D\u092F\u094B\u0902 \u0915\u093E \u0938\u0902\u091A\
  \u093E\u0932\u0928 \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902."
title: "\u0915\u094B\u0921 \u0915\u094B \u092B\u093C\u0902\u0915\u094D\u0936\u0928\
  \u094D\u0938 \u092E\u0947\u0902 \u0935\u094D\u092F\u0935\u0938\u094D\u0925\u093F\
  \u0924 \u0915\u0930\u0928\u093E"
weight: 18
---

## कैसे करें:
```Lua
-- एक साधारण फंक्शन को परिभाषित करें जो अभिवादन करे
function greet(name)
    return "Hello, " .. name .. "!"
end

-- फंक्शन का उपयोग करें
print(greet("Lua प्रोग्रामर")) -- नमूना आउटपुट: Hello, Lua प्रोग्रामर!
```

फंक्शन जटिल हो सकते हैं, विभिन्न कार्यों का संचालन कर सकते हैं:
```Lua
-- एक फंक्शन जो एक आयत का क्षेत्रफल गणना करता है
function calculateArea(width, height)
    return width * height
end

-- फंक्शन को कॉल करें और परिणाम को प्रिंट करें
local area = calculateArea(5, 4)
print(area)  -- नमूना आउटपुट: 20
```

## गहराई से अध्ययन
Lua, इसके आरंभ से 90s में, मॉड्यूलर डिज़ाइन को प्रोत्साहित करता रहा है। कोड को फंक्शंस के साथ व्यवस्थित करना सिर्फ Lua के लिए अनूठा नहीं है—यह फोरट्रान और लिस्प जैसी प्रोग्रामिंग भाषाओं के उदय से अभ्यास में रहा है। इनलाइन कोड की तरह के विकल्प और उसी कोड को बार-बार कॉपी और पेस्ट करना न केवल अवांछित है; ये संभावित बग के घोंसले हैं।

Lua में, फंक्शंस प्रथम श्रेणी के नागरिक हैं, अर्थात इन्हें वेरिएबल्स में संग्रहित किया जा सकता है, तर्क के रूप में पास किया जा सकता है, और अन्य फंक्शंस से वापस लौटाया जा सकता है। ये बहुउद्देशीय हैं। Lua की सिंगल-थ्रेडेड प्रकृति का अर्थ है कि आपको प्रदर्शन के लिए फंक्शंस को संतुलित और प्रभावी रखना होगा। फंक्शंस स्थानीय (स्कोप्ड) या वैश्विक हो सकते हैं, और कब कौन सा उपयोग करना है, यह जानना आपके स्क्रिप्ट की कुशलता को बना या बिगाड़ सकता है।

## देखें भी
- फंक्शंस पर आधिकारिक Lua दस्तावेज़ीकरण: https://www.lua.org/pil/6.html
- Lua में फंक्शन उपयोग के व्यावहारिक उदाहरण: https://lua-users.org/wiki/SampleCode
- Lua में स्वच्छ कोड प्रथाओं: https://github.com/Olivine-Labs/lua-style-guide
