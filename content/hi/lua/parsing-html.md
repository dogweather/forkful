---
title:                "HTML पार्स करना"
date:                  2024-01-20T15:33:30.500863-07:00
html_title:           "Bash: HTML पार्स करना"
simple_title:         "HTML पार्स करना"

category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
HTML पार्सिंग एक प्रक्रिया है जिसमें HTML कोड को समझकर उसकी संरचना का अध्ययन किया जाता है। प्रोग्रामर इसे वेब पेज का डेटा एक्सट्रैक्ट करने, स्क्रैपिंग, या ऑटोमेशन के लिए करते हैं।

## How to: (कैसे करें:)
Lua में HTML पार्सिंग के लिए एक लोकप्रिय बाहरी लाइब्रेरी `luasocket` का `http` मॉड्यूल और `lxp` (Lua XML Parser) इस्तेमाल किया जा सकता है। आइए एक साधारण उदाहरण देखें:

```Lua
local http = require("socket.http")
local lxp = require("lxp")

-- HTML कंटेंट को डाउनलोड करें
local content = assert(http.request("http://www.example.com"))

-- पार्सिंग के लिए साधारण हैंडलर
local function StartElementParser(parser, elementName, attributes)
  if elementName == "a" then
    print("Link found:", attributes.href)
  end
end

-- XML पार्सर इनिशियलाइज करें
local p = lxp.new({StartElement=StartElementParser})

-- HTML को पार्स करें
p:parse(content)
p:parse() -- पार्सिंग समाप्त करें
p:close() -- XML पार्सर को बंद करें
```

उदाहरण का आउटपुट:

```
Link found: http://www.iana.org/domains/example
```

## Deep Dive (गहराई में जानकारी)
HTML पार्सिंग का इतिहास वेब स्क्रैपिंग और वेब इंडेक्सिंग के साथ चलता है। जहाँ एक ओर लुआ इस काम के लिए एक असाधारण भाषा नहीं है, वहीं इसके मॉड्यूलर अप्रोच और हल्के वजन ने इसे स्क्रिप्टिंग और एम्बेडेड सिस्टम्स में लोकप्रिय बनाया है। `lxp` मॉड्यूल एक्सएमएल पार्सिंग के लिए है, लेकिन HTML को भी अच्छे से हैंडल कर सकता है। एक अल्टरनेटिव के रूप में, `luasocket` का `http` मॉड्यूल का इस्तेमाल करते हुए सीधे रेगुलर एक्सप्रेशंस के साथ HTML को प्रोसेस किया जा सकता है, पर यह जटिल हो सकता है और HTML के मानकों को ठीक से नहीं समझा जाता।

## See Also (और भी देखें)
- HTML पार्सिंग की बेसिक अवधारणाओं पर लेख: [HTML Parsing](https://html.spec.whatwg.org/multipage/parsing.html)
