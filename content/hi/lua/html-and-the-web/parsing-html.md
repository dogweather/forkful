---
title:                "HTML विश्लेषण"
date:                  2024-02-03T19:13:35.680356-07:00
model:                 gpt-4-0125-preview
simple_title:         "HTML विश्लेषण"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/parsing-html.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
HTML को पार्स करने का अर्थ है HTML दस्तावेजों से डेटा और जानकारी को निकालना, जो वेब स्क्रैपिंग, डेटा विश्लेषण, और ऑटोमेशन कार्यों के लिए महत्वपूर्ण है। प्रोग्रामर्स इसे वेब सामग्री को स्वचालित रूप से इकट्ठा करने, विश्लेषण करने, या हेरफेर करने के लिए करते हैं, जो वेबसाइटों से डेटा के मैनुअल निष्कर्षण को स्वचालित बनाता है।

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