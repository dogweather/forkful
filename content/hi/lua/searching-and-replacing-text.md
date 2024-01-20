---
title:                "पाठ की खोज और प्रतिस्थापन"
html_title:           "Bash: पाठ की खोज और प्रतिस्थापन"
simple_title:         "पाठ की खोज और प्रतिस्थापन"
programming_language: "Lua"
category:             "Lua"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/lua/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

खोजना और बदलना सादा एक प्रोग्रामिंग के तरीके हैं वाकय को पिछानने और ऐडिट करने के लिए। प्रोग्रामर इसे कोड के टुकड़ों को पुन: उपयोग करने, त्रुटियाँ सुधारने और डाटा को अनुकूलित करने के लिए करते हैं। 

## कैसे करें:

Lua में टेक्स्ट खोजने और बदलने के लिए `string.gsub` फ़ंक्शन का उपयोग करें। इसका उपयोग निम्नलिखित तरीके से किया जा सकता है:

```Lua
text = "नमस्ते, दुनिया!"
text = string.gsub(text, "नमस्ते", "अलविदा")
print(text) -- अलविदा, दुनिया!
```

## गहराई में:

1. ऐतिहासिक प्रसंग: Lua का पहला संस्करण, जो 1993 में जारी हुआ, ने string.gsub फ़ंक्शन को पहली बार परिभाषित किया था।
2. विकल्प: Regex भी Lua में खोज और बदलाव करने का एक विकल्प है, जो अधिक काम्याबी और शक्तिशाली हो सकता है, लेकिन अधिक जटिल है।
3. क्रियान्वयन विवरणी: `gsub` फ़ंक्शन पहले string में पैटर्न की खोज करता है, फिर उसे नई string के साथ बदलता है। यह अपने आप में एक loop की तरह काम करता है।

## अन्य स्रोत देखें:

1. Lua 5.3 रेफरेंस मैनुअल: [string.gsub](https://www.lua.org/manual/5.3/manual.html#pdf-string.gsub)
2. Lua-Users विकी: [StringLibraryTutorial](http://lua-users.org/wiki/StringLibraryTutorial)