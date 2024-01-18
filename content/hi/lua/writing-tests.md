---
title:                "टेस्ट लिखना"
html_title:           "Lua: टेस्ट लिखना"
simple_title:         "टेस्ट लिखना"
programming_language: "Lua"
category:             "Lua"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/writing-tests.md"
---

{{< edit_this_page >}}

## Lua में टेस्ट लिखना क्या है और क्यों करें?

Lua में टेस्ट लिखना एक प्रोग्रामर के नियमित कामों में से एक है। यह कोड को भलीभांति चलाने और निश्चित करने के लिए आवश्यक होता है कि उसमें कोई ग़लती ना हो। टेस्ट लिखना ऐसी प्रक्रिया है जो आपको आपके कोड की ग़लतियों को पहचानने में मदद करती है और उन्हें सुधारने में भी। 

## कैसे:

```Lua
-- एक सरल टेस्ट कोड का उदाहरण
if add(2,3) == 5 then
  print("यह टेस्ट सफल है!")
else
  print("इस टेस्ट में कोई ग़लती है।")
end
```
**उत्पादन:**

यह टेस्ट सफल है!

## गहराई में जाएं:

क्या आप जानते हैं कि Lua में टेस्ट लिखने की प्रथम रचनाओं में से एक "lunit" नामक टूल था? इसके अलावा, आप Unit Testing Frameworks जैसे "luaunit" या "busted" का उपयोग करके भी टेस्ट लिख सकते हैं। प्रोग्रामिंग की दुनिया में "ख़राब" कोडों को ठीक करने की विधि को "Test Driven Development (TDD)" कहते हैं और टेस्ट लिखना इस प्रक्रिया का एक महत्वपूर्ण हिस्सा है। 

## इसके अलावा देखें:

- [Lua में टेस्ट लिखने के बारे में जानकारी](https://www.lua.org/pil/11.html)
- [Lua में "lunit" टेस्टिंग टूल का उपयोग](https://luaunit.readthedocs.io)
- [Lua में "busted" टूल का उपयोग](https://olivinelabs.com/busted)