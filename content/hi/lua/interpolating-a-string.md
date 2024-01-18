---
title:                "स्ट्रिंग इंटरपोलेशन"
html_title:           "Lua: स्ट्रिंग इंटरपोलेशन"
simple_title:         "स्ट्रिंग इंटरपोलेशन"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/interpolating-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

स्ट्रिंग इंटर्पोलेशन एक उपयोगी प्रोग्रामिंग तकनीक है जिसका उपयोग स्ट्रिंग में अन्य जानकारी के साथ जानकारी जोड़ने के लिए किया जाता है। कई बार प्रोग्रामिंग जीवन में हमें किसी स्ट्रिंग को अन्य जानकारी के साथ प्रिंट करने की जरूरत पड़ती है, जैसे एक वेरिएबल या किसी अन्य स्ट्रिंग। स्ट्रिंग इंटर्पोलेशन इस समस्या को हल करता है और हमें स्ट्रिंग में इन जानकारियों को सही ढंग से प्रिंट करने की सुविधा प्रदान करता है।

## कैसे करें:

```Lua
-- "Hello" और "Hindi readers" डाले गए स्ट्रिंग हैं
print("Hello, Hindi readers!")

-- इस स्ट्रिंग में वेरिएबल 'name' का मान जोड़ना है
local name = "Sarah"
print("Hello, my name is " .. name)

-- स्ट्रिंग के भीतर मूल्यों को गणितीय ऑपरेटर के साथ भी जोड़ा जा सकता है
local num1 = 10
local num2 = 5
print("Sum is: " .. (num1 + num2))
```

आउटपुट:

```Lua
Hello, Hindi readers!
Hello, my name is Sarah
Sum is: 15
```

## डीप डाइव:

इस तकनीक का इतिहास लूआ भाषा की शुरुआत से है। स्ट्रिंग इंटर्पोलेशन को चरित्रावली में ".." ऑपरेटर से दर्शाया गया है। इसे अन्य भाषाओं में भी दो स्ट्रिंग्स को आसानी से जोड़ने के लिए उपयोग किया जाता है। हालांकि, स्ट्रिंग इंटर्पोलेशन का उपयोग उसी मामले में किया जाता है जब हम स्ट्रिंग के भीतर मूल्यों को अन्य समस्याओं को हल करने के लिए भी करते हैं।

## देखें भी:

- [Official Lua Documentation](https://www.lua.org/manual/5.4/manual.html#3.4.6)
- [String Interpolation in other languages](https://en.wikipedia.org/wiki/String_interpolation#Other_languages)