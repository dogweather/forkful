---
title:                "HTML पार्स करना"
html_title:           "C++: HTML पार्स करना"
simple_title:         "HTML पार्स करना"
programming_language: "Lua"
category:             "Lua"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/parsing-html.md"
---

{{< edit_this_page >}}

## क्या & क्यों?
HTML पार्सिंग का मतलब है HTML डॉक्युमेंट को विश्लेषित करना, जिसे प्रोग्रामर्स वेबसाइट की संरचना और डाटा को समझने और प्रबंधित करने के लिए करते हैं।

## कैसे करें:
```Lua
local htmlparser = require "htmlparser"
local root = htmlparser.parse('<html><body>Hello World!</body></html>')
root:select('body')[1]:getcontent()
```
आउटपुट:
```
Hello World!
```
## गहरा डाइव
HTML पार्सिंग का इतिहास में अनेक चरण रहे हैं, पहले इसे ऑटोमेशन और डाटा माइनिंग के लिए किया जाता था, अब इसका उपयोग वेब ऐप्लिकेशन में डाटा प्रसंस्करण और मैनीपुलेशन के लिए होता है। वैकल्पिक तरीके जैसे कि RegEx का उपयोग किया जा सकता है, लेकिन वे HTML के जटिल संरचना को सही तरीके से पार्स नहीं कर पाते। HTML पार्सर मूल रूप से HTML टैग्स, विशेषताओं और पाठ्य को विश्लेषित करके, उन्हें प्रक्रियायोग्य आंतरिक संरचना में परिवर्तित करते हैं।

## देखने योग्य
1. ['Lua-users'](https://lua-users.org/wiki/)
2. [Programming in Lua (book)](https://www.lua.org/pil/)
3. [Official Lua Documentation](https://www.lua.org/manual/5.4/manual.html)
4. [HTML Parsing Libraries for Lua](https://lua-toolbox.com/categories/html_parsing.html)