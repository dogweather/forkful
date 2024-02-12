---
title:                "स्ट्रिंग से उद्धरण चिह्न हटाना"
date:                  2024-01-26T03:43:06.451602-07:00
model:                 gpt-4-0125-preview
simple_title:         "स्ट्रिंग से उद्धरण चिह्न हटाना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/removing-quotes-from-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
एक स्ट्रिंग से कोट्स (उद्धरण) हटाना का मतलब है आपके टेक्स्ट को घेरे हुए उन डबल या सिंगल कोट्स के कैरेक्टर्स को निकालना। कोडर्स इसे इनपुट्स को सैनिटाइज करने के लिए, पार्सिंग को आसान बनाने के लिए, या डेटा को सामंजस्य बनाने के लिए करते हैं जो शायद असंगत रूप से कोटेड हो सकता है।

## कैसे करें:
यहाँ Lua में कोट्स को किनारे करने का तरीका है:

```lua
local function remove_quotes(str)
  return (str:gsub("^%p(.*)%p$", "%1"))
end

print(remove_quotes('"Hello, World!"'))     -- Hello, World!
print(remove_quotes("'Goodbye, Quotes!'"))  -- Goodbye, Quotes!
```

बिंगो! वो कोट्स ऐसे गायब हो गए जैसे ड्रायर में मोजे।

## गहराई में जानें
लोग स्ट्रिंग से कोट्स हटा रहे हैं जब से भाषाएँ टेक्स्ट को संभाल पा रही हों, जो लगभग हमेशा से है। Lua में, `gsub` फ़ंक्शन भारी उठाने का काम करता है, पैटर्न्स को एक स्कैल्पल की तरह उठाकर कोट्स को हटा देता है। विकल्प? ज़रूर, आप उन भाषाओं में रेगेक्स के साथ जा सकते हैं जो इसका समर्थन करती हैं, या अपना खुद का लूप लिख सकते हैं जो प्रत्येक कैरेक्टर को चबाता है (ऊँह, पर हेय, यह आपका समय है)।

Lua की पैटर्न मैचिंग आपको पूरे लाइब्रेरी को इम्पोर्ट किए बिना रेगेक्स-लाइट अनुभव की ताकत देती है। कैरेट (`^`) और डॉलर साइन (`$`) क्रमशः स्ट्रिंग के शुरुआत और अंत को मैच करते हैं; `%p` किसी भी विराम चिह्न कैरेक्टर को मैच करता है। अग्रणी और पिछला विराम झटकने के बाद, हम `(.*),` से सब कुछ पकड़ लेते हैं, और पूरे मैच को उस कैप्चर ग्रुप के साथ बदल देते हैं जिसका उपयोग `" %1"` करते हैं।

ध्यान रखें कि Lua की पैटर्न मैचिंग पूर्ण-उड़ान रेगेक्स इंजनों के रूप में शक्तिशाली नहीं है - उदाहरण के लिए, यह गिन सकता नहीं है या बैकट्रैक नहीं कर सकता। यह सादगी एक आशीर्वाद और अभिशाप दोनों है, यह निर्भर करता है कि आप किन कोट्स को संभाल रहे हैं और वे कहाँ छिपे हुए हैं।

## भी देखें
Lua की पैटर्न मैचिंग में गहराई से उतरने के लिए PiL (Programming in Lua) पुस्तक को देखें: http://www.lua.org/pil/20.2.html

शुद्ध एलिगेंसी के लिए, तुलना के लिए अन्य भाषाओं में यह कैसे किया जाता है, इसका पता लगाएं, Python के `str.strip` से शुरू करें: https://docs.python.org/3/library/stdtypes.html#str.strip