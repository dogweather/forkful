---
date: 2024-01-20 17:44:39.578512-07:00
description: "\u0935\u0947\u092C \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\
  \u094B\u0921 \u0915\u0930\u0928\u093E \u092E\u0924\u0932\u092C \u0907\u0902\u091F\
  \u0930\u0928\u0947\u091F \u0938\u0947 \u0938\u0942\u091A\u0928\u093E \u0932\u0947\
  \u0915\u0930 \u0909\u0938\u0947 \u0905\u092A\u0928\u0947 \u0915\u0902\u092A\u094D\
  \u092F\u0942\u091F\u0930 \u092A\u0930 \u0938\u0947\u0935 \u0915\u0930\u0928\u093E\
  \u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938\
  \ \u0907\u0938\u0947 \u0921\u093E\u091F\u093E \u090F\u0928\u093E\u0932\u093F\u0938\
  \u093F\u0938, \u091F\u0947\u0938\u094D\u091F\u093F\u0902\u0917 \u0914\u0930 \u0935\
  \u0947\u092C \u0938\u093E\u0907\u091F \u0915\u0940 \u0915\u0902\u091F\u0947\u0902\
  \u091F \u092E\u0949\u0928\u093F\u091F\u0930\u093F\u0902\u0917 \u0915\u0947\u2026"
lastmod: 2024-02-19 22:05:11.562384
model: gpt-4-1106-preview
summary: "\u0935\u0947\u092C \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\u094B\
  \u0921 \u0915\u0930\u0928\u093E \u092E\u0924\u0932\u092C \u0907\u0902\u091F\u0930\
  \u0928\u0947\u091F \u0938\u0947 \u0938\u0942\u091A\u0928\u093E \u0932\u0947\u0915\
  \u0930 \u0909\u0938\u0947 \u0905\u092A\u0928\u0947 \u0915\u0902\u092A\u094D\u092F\
  \u0942\u091F\u0930 \u092A\u0930 \u0938\u0947\u0935 \u0915\u0930\u0928\u093E\u0964\
  \ \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0907\
  \u0938\u0947 \u0921\u093E\u091F\u093E \u090F\u0928\u093E\u0932\u093F\u0938\u093F\
  \u0938, \u091F\u0947\u0938\u094D\u091F\u093F\u0902\u0917 \u0914\u0930 \u0935\u0947\
  \u092C \u0938\u093E\u0907\u091F \u0915\u0940 \u0915\u0902\u091F\u0947\u0902\u091F\
  \ \u092E\u0949\u0928\u093F\u091F\u0930\u093F\u0902\u0917 \u0915\u0947\u2026"
title: "\u0935\u0947\u092C \u092A\u0947\u091C \u0921\u093E\u0909\u0928\u0932\u094B\
  \u0921 \u0915\u0930\u0928\u093E"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

वेब पेज डाउनलोड करना मतलब इंटरनेट से सूचना लेकर उसे अपने कंप्यूटर पर सेव करना। प्रोग्रामर्स इसे डाटा एनालिसिस, टेस्टिंग और वेब साइट की कंटेंट मॉनिटरिंग के लिए करते हैं।

## How to: (कैसे करें:)

Lua में वेब पेज डाउनलोड करने के लिए सबसे सीधा तरीका `socket.http` मॉड्यूल का इस्तेमाल करना है। इसे `luasocket` लाइब्रेरी के तौर पर इंस्टॉल करना पड़ता है।

```Lua
-- सबसे पहले luasocket लाइब्रेरी को इंस्टॉल करें
-- यह कमांड इस्तेमाल कर सकते हैं: luasocket install luarocks

local http = require("socket.http")
local body, statusCode, headers, statusText = http.request("http://www.example.com")

if statusCode == 200 then
    print("Web page downloaded successfully!")
    print(body) -- वेब पेज का HTML कोड
else
    print("Error downloading web page:", statusText)
end
```

Sample Output (नमूना आउटपुट):

```
Web page downloaded successfully!
<!DOCTYPE html>...
```

## Deep Dive (गहराई से समझें):

पहले जमाने में, वेब पेज डाउनलोड करना बहोत जटिल था। लेकिन आज, `luasocket` जैसे लाइब्रेरीज ने इसे बहोत सरल बना दिया है। हालांकि `socket.http` बहुत ही बुनियादी है, और यह सिर्फ HTTP GET रिक्वेस्ट को हैंडल कर सकता है। अगर आपको POST रिक्वेस्ट या कुकीज़ को मैनेज करना है, तो आपको `luasec` जो कि `luasocket` पर आधारित है, या किसी और अधिक एडवांस लाइब्रेरी की जरुरत होगी।

आप वेब स्क्रेपिंग के लिए `Lua` के साथ-साथ दूसरे ऑप्शन्स भी तलाश सकते हैं, जैसे कि `Python` के `BeautifulSoup` या `Scrapy`। लेकिन Lua का सादगी और परफॉरमेंस में उत्कृष्टता इसे कुछ अनुप्रयोगों के लिए एक बेहतर पसंद बनाते हैं।

## See Also (देखें भी):

- LuaSocket reference: [http://w3.impa.br/~diego/software/luasocket/http.html](http://w3.impa.br/~diego/software/luasocket/http.html)
- LuaSec for HTTPS support: [https://github.com/brunoos/luasec](https://github.com/brunoos/luasec)
- Lua programming homepage: [https://www.lua.org/](https://www.lua.org/)
- More on web scraping with Lua: [https://www.scraperwiki.com/](https://www.scraperwiki.com/)
