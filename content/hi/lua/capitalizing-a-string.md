---
title:                "स्ट्रिंग को कैपिटलाइज़ करना"
date:                  2024-01-19
simple_title:         "स्ट्रिंग को कैपिटलाइज़ करना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
स्ट्रिंग का प्रथम अक्षर बड़ा करने को Capitalizing कहते हैं। यह व्याकरणिक सहीपन के लिए और लिखावट में सुधार के लिए प्रोग्रामर्स करते हैं।

## How to: (कैसे करें:)
```Lua
function capitalize(str)
    return str:sub(1,1):upper() .. str:sub(2)
end

-- उदाहरण
print(capitalize("hello")) -- Hello
print(capitalize("नमस्ते")) -- नमस्ते
```

## Deep Dive (गहराई से जानिए)
Lua में, स्ट्रिंग का पहला अक्षर बड़ा करना सीधा है। `string.upper` फंक्शन पूरी स्ट्रिंग को कैपिटल लैटर्स में बदलता है, जबकि `string.sub` स्ट्रिंग का एक हिस्सा निकालता है। पहले का पहला अक्षर `string.sub(str,1,1)` से बड़ा करके `string.upper` अप्लाई करते हैं, फिर बाकी स्ट्रिंग `string.sub(str,2)` को जोड़ते हैं।

स्ट्रिंग Capitalization का उपयोग लोगों के नाम, खिताब, या पहली बार शब्द प्रदर्शित करते समय होता है। इसका उपयोग यूजर इंटरफ़ेस और प्रिंटेड टेक्स्ट में प्रमुखता के लिए भी होता है।

विकल्प के रूप में, कुछ बाहरी लाइब्रेरी भी होती हैं जो मोर कॉम्प्लेक्स टेक्स्ट मनिप्युलेशन फंक्शन्स प्रदान करती हैं, जैसे की `penlight`.

## See Also (और भी जानकारी)
- Lua मानक लाइब्रेरी डाक्यूमेंटेशन: https://www.lua.org/manual/5.4/manual.html#6.4
- Lua 'Penlight' लाइब्रेरी: https://github.com/lunarmodules/Penlight
