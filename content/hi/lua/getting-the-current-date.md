---
title:                "वर्तमान तारीख प्राप्त करना"
html_title:           "C#: वर्तमान तारीख प्राप्त करना"
simple_title:         "वर्तमान तारीख प्राप्त करना"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/getting-the-current-date.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)

वर्तमान दिनांक पाने का उद्देश्य यह है कि उसे किसी specific task कैलेंडार जैसी ऐप्लिकेशन में प्रयोग हो सके । कार्यान्वयन करने वाले डेटा, समयित संग्रहण या उपयोगी डेबगिंग जानकारी जैसी चीज़ों के लिए वर्तमान दिनांक को प्राप्त करना संभावित है।

## कैसे (How to)

Lua में, कोड स्निपेट आपको वर्तमान दिनांक प्राप्त करने में मदद करेगा:

```Lua
os.date("%x")
```

उपर्युक्त कोड को निष्पादित करने पर, आपको मानक दिनांक प्राप्त होगा, जैसा कि:

```
01/31/22
```

## गहरी जानकारी (Deep Dive)

Lua में “os.date()” का प्रयोग करके वर्तमान दिनांक प्राप्त किया जा सकता है, इसमें आप ISO C के समय मानदंडों के अनुसार आउटपुट को फॉर्मेट कर सकते हैं। 

दूसरा उपाय सीआर्टी "time()” और "localtime()" इस्तेमाल करना हो सकता है। लेकिन os.date() अधिक व्यापक, फ्लेक्सिबल, और प्रतिबद्धक-मुक्त है, जो प्रोग्रामर्स के लिए एक बड़ा लाभ हो सकता है। 

यहाँ एक अन्य प्रयास:

```Lua
os.time(os.date("*t"))
```

इसे चलाने पर:

```
1643729940
```

## और देखें (See Also)

Lua के अधिकारिक दस्तावेज़ (Official Lua Documentation): http://www.lua.org/manual/5.4/ 

Programming in Lua (Third Edition) by Roberto Ierusalimschy https://www.lua.org/pil/

Stack Overflow | Lua Date and Time: https://stackoverflow.com/questions/36025672/how-to-get-current-date-and-time-in-lua