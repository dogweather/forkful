---
title:                "दो तारीखों की तुलना"
html_title:           "Elixir: दो तारीखों की तुलना"
simple_title:         "दो तारीखों की तुलना"
programming_language: "Lua"
category:             "Lua"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/comparing-two-dates.md"
---

{{< edit_this_page >}}

# यह क्या है और क्यों? 
दो तारीखों की तुलना, जैसा की नाम से ही पता चलता है, दो तारीखों को एक-दूसरे के साथ तुलना करना है. प्रोग्रामर्स इसे उपयोग करते हैं क्योंकि आमतौर पर एप्लिकेशन्स में किसी अवधि या समय अवधि की गणना करने की आवश्यकता होती है.

# कैसे करें: 
Lua में, दो तारीखों की तुलना करने के लिए आप इसे इस तरह कर सकते हैं: 

```Lua
local time1 = os.time{year=2022, month=5, day=10}
local time2 = os.time{year=2022, month=5, day=15}

if (time1 < time2) then
    print("Date 1 is earlier than Date 2")
else
    print("Date 1 is not earlier than Date 2")
end
```

आउटपुट:

```
Date 1 is earlier than Date 2
```

# गहराई में: 
(1) ऐतिहासिक संदर्भ: Lua में तारीख की गणना का यह सिस्टम 1993 में Lua 1.0 के पहले संस्करण के साथ ही शुरू हुआ। (2) विकल्प: Lua के अलावा, प्रोग्रामर्स Python, JavaScript जैसी भाषाओं का उपयोग भी कर सकते हैं जिनमें डेट की गणना और तुलना के लिए built-in libraries होती हैं। (3) कार्यान्वयन विवरण: Lua में, `os.time` फ़ंक्शन Unix timestamp प्रदान करता है जिसे हम तारीखों की तुलना करने के लिए उपयोग कर सकते हैं। 

# देखने के लिए भी: 
अधिक जानकारी के लिए, निम्नलिखित संसाधनों पर जाएँ:

1. [Lua 5.3 रेफरेंस मैनुअल](http://www.lua.org/manual/5.3/)
3. [Lua उपयोगकर्ता विकी - os.date और os.time](http://lua-users.org/wiki/OsLibraryTutorial)