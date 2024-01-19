---
title:                "रेगुलर एक्सप्रेशन का प्रयोग करना"
html_title:           "Lua: रेगुलर एक्सप्रेशन का प्रयोग करना"
simple_title:         "रेगुलर एक्सप्रेशन का प्रयोग करना"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/using-regular-expressions.md"
---

{{< edit_this_page >}}

## ये क्या है और क्यों करें?

लुआ में नियमित अभिव्यक्तियों का उपयोग करना हमें पाठकों को कहां और कैसे खोजें, परिवर्तन करें और मैच करें के लिए सक्षम बनाता है। ये मूल रूप से मैचिंग और परिवर्तन के लिए तरीके हैं जो कुछ स्ट्रिंग पर आधारित होते हैं। इससे हमें अधिक समझ मिलती है कि कैसे कठिन से कठिन स्ट्रिंग भी आसानी से मिल सकती है। यह दर्शाता है कि इन नियमित अभिव्यक्तियों का उपयोग क्यों किया जाता है।

## कैसे करें:

यहां, हम लुआ में नियमित अभिव्यक्तियों का उपयोग करने के तरीके की बात करेंगे।
 
```Lua
-- स्ट्रिंग का मैच ढूंढे
local text = "Hello World!"
local pattern = "%a+" -- कोई भी अक्षरिक अवधा स्ट्रिंग
local match = string.match(text, pattern)
print(match) --> "Hello"

-- स्ट्रिंग पर बादल ऑटपुट
local text = "Hello World!"
local pattern = "(%a+)%s(%a+)"
local replace = string.gsub(text, pattern, "Bye %2")
print(replace) --> "Bye World!"

-- पूरे स्ट्रिंग को बदलें
local text = "Hello World!"
local pattern = "%a+"
local replace = string.gsub(text, pattern, "Hi")
print(replace) --> "Hi Hi!"
```

## गहराई में खुजली करें:

नियमित अभिव्यक्तियों का उपयोग करने के अन्य विकल्प भी हैं, जैसे कि मैच और अनुरोध के लिए उपयोगी लूआ लाइब्रेरी। यह भी कम्प्लेक्स स्ट्रिंग प्रोसेसिंग के साथ सहायता कर सकती है।

दरअसल, लुआ में नियमित अभिव्यक्तियों का अनुरोधित प्रश्नक भी हैं जो वीएम मे पूरे होते हैं। मूल तरफ से, वनको शील्ड में भी उपलब्ध होते हैं। काम करते समय भी आपको अनुमति है कि आप वन द्वारा निर्वाचित अनुरोध भूमिका बनाएं, जो प्राप्ति के दौरान सम्पूर्ण अनुरोध ले आता है। इसके अन्दर वन द्वारा संचालन किया जा सकता है और जब हम लीसन लागू होताहै, तब दो देखने और स्थिर करनी के लिए वनको वि कोल कर स्कते हैं।

## कुछ और भी देखें:

 - [लुआ दस्तावेज़ीकरण]: https://www.lua.org/manual/5.3/manual.html
 - [गड्ढे में नियमित अभिव्यक्तियों : // hackernoon.com/a-basic-guide-to-regular-expressions-in-lua-890e9f56fdfe
 - [पावरेक्स के लिए लुआ]: https://www.lua.org/pil/20.2.html