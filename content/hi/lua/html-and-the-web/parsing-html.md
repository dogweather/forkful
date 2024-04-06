---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:13:35.680356-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: Lua \u092E\u0947\u0902\
  \ HTML \u0915\u094B \u092A\u093E\u0930\u094D\u0938 \u0915\u0930\u0928\u0947 \u0915\
  \u0947 \u0932\u093F\u090F \u092C\u093F\u0932\u094D\u091F-\u0907\u0928 \u0932\u093E\
  \u0907\u092C\u094D\u0930\u0947\u0930\u0940 \u0928\u0939\u0940\u0902 \u0939\u0948\
  , \u092A\u0930 \u0906\u092A `LuaHTML` \u091C\u0948\u0938\u0940 \u0925\u0930\u094D\
  \u0921-\u092A\u093E\u0930\u094D\u091F\u0940 \u0932\u093E\u0907\u092C\u094D\u0930\
  \u0947\u0930\u0940\u091C \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\
  \ \u0938\u0915\u0924\u0947 \u0939\u0948\u0902 \u092F\u093E `LuaXML` \u0915\u0947\
  \u2026"
lastmod: '2024-04-05T21:53:54.524453-06:00'
model: gpt-4-0125-preview
summary: "Lua \u092E\u0947\u0902 HTML \u0915\u094B \u092A\u093E\u0930\u094D\u0938\
  \ \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u092C\u093F\u0932\u094D\
  \u091F-\u0907\u0928 \u0932\u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940 \u0928\
  \u0939\u0940\u0902 \u0939\u0948, \u092A\u0930 \u0906\u092A `LuaHTML` \u091C\u0948\
  \u0938\u0940 \u0925\u0930\u094D\u0921-\u092A\u093E\u0930\u094D\u091F\u0940 \u0932\
  \u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940\u091C \u0915\u093E \u0909\u092A\
  \u092F\u094B\u0917 \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902 \u092F\
  \u093E `LuaXML` \u0915\u0947 \u092E\u093E\u0927\u094D\u092F\u092E \u0938\u0947 `libxml2`\
  \ \u0915\u0947 \u092C\u093E\u0907\u0902\u0921\u093F\u0902\u0917\u094D\u0938 \u0915\
  \u093E \u0932\u093E\u092D \u0909\u0920\u093E \u0938\u0915\u0924\u0947 \u0939\u0948\
  \u0902\u0964 \u090F\u0915 \u0932\u094B\u0915\u092A\u094D\u0930\u093F\u092F \u0926\
  \u0943\u0937\u094D\u091F\u093F\u0915\u094B\u0923 HTML \u0915\u094B \u092A\u093E\u0930\
  \u094D\u0938 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F `lua-gumbo`\
  \ \u0932\u093E\u0907\u092C\u094D\u0930\u0947\u0930\u0940 \u0915\u093E \u0909\u092A\
  \u092F\u094B\u0917 \u0915\u0930\u0928\u093E \u0939\u0948, \u091C\u094B \u0938\u0940\
  \u0927\u0947, HTML5-\u0905\u0928\u0941\u0930\u0942\u092A \u092A\u093E\u0930\u094D\
  \u0938\u093F\u0902\u0917 \u0915\u094D\u0937\u092E\u0924\u093E \u092A\u094D\u0930\
  \u0926\u093E\u0928 \u0915\u0930\u0924\u093E \u0939\u0948\u0964."
title: "HTML \u0935\u093F\u0936\u094D\u0932\u0947\u0937\u0923"
weight: 43
---

## कैसे करें:
Lua में HTML को पार्स करने के लिए बिल्ट-इन लाइब्रेरी नहीं है, पर आप `LuaHTML` जैसी थर्ड-पार्टी लाइब्रेरीज का उपयोग कर सकते हैं या `LuaXML` के माध्यम से `libxml2` के बाइंडिंग्स का लाभ उठा सकते हैं। एक लोकप्रिय दृष्टिकोण HTML को पार्स करने के लिए `lua-gumbo` लाइब्रेरी का उपयोग करना है, जो सीधे, HTML5-अनुरूप पार्सिंग क्षमता प्रदान करता है।

### lua-gumbo इंस्टॉल करना:
सबसे पहले, सुनिश्चित करें कि `lua-gumbo` इंस्टॉल है। आप आमतौर पर इसे luarocks का उपयोग करके इंस्टॉल कर सकते हैं:

```sh
luarocks install lua-gumbo
```

### lua-gumbo के साथ मूल पार्सिंग:
यहाँ आप कैसे `lua-gumbo` का उपयोग करके एक साधारण HTML स्निपेट को पार्स कर सकते हैं और इससे डेटा निकाल सकते हैं:

```lua
local gumbo = require "gumbo"
local document = gumbo.parse[[<html><body><p>Hello, world!</p></body></html>]]

local p = document:getElementsByTagName("p")[1]
print(p.textContent)  -- आउटपुट: Hello, world!
```

### उन्नत उदाहरण - लिंक्स निकालना:
एक HTML दस्तावेज़ में सभी एंकर टैग्स (`<a>` तत्वों) से `href` विशेषताएं निकालने के लिए:

```lua
local gumbo = require "gumbo"
local document = gumbo.parse([[
<html>
<head><title>Sample Page</title></head>
<body>
  <a href="http://example.com/1">Link 1</a>
  <a href="http://example.com/2">Link 2</a>
  <a href="http://example.com/3">Link 3</a>
</body>
</html>
]])

for _, element in ipairs(document.links) do
    if element.getAttribute then  -- सुनिश्चित करें कि यह एक तत्व है और इसमें विशेषताएं हैं
        local href = element:getAttribute("href")
        if href then print(href) end
    end
end

-- नमूना आउटपुट:
-- http://example.com/1
-- http://example.com/2
-- http://example.com/3
```

यह कोड स्निपेट दस्तावेज़ में सभी लिंक्स के माध्यम से चक्रित करता है और उनके `href` विशेषताओं को प्रिंट करता है। `lua-gumbo` लाइब्रेरी की एक HTML दस्तावेज़ की संरचना को पार्स करने और समझने की क्षमता तत्वों को उनके टैग्स या विशेषताओं के आधार पर निकालने की प्रक्रिया को सरल बनाती है।
