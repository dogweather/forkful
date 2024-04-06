---
date: 2024-01-20 17:42:43.249694-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) ."
lastmod: '2024-04-05T21:53:54.504916-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u092A\u0948\u091F\u0930\u094D\u0928 \u0938\u0947 \u092E\u0947\u0932 \u0916\
  \u093E\u0924\u0947 \u0905\u0915\u094D\u0937\u0930\u094B\u0902 \u0915\u094B \u0939\
  \u091F\u093E\u0928\u093E"
weight: 5
---

## How to: (कैसे करें:)
```Lua
-- स्ट्रिंग से विशिष्ट पैटर्न को हटाने का उदाहरण
local original_str = "हेलो123_वर्ल्ड"
local pattern = "%d+" -- अंकों को मैच करने वाला पैटर्न
local result_str = original_str:gsub(pattern, "")
print(result_str) -- हेलो_वर्ल्ड
```

```Lua
-- पंक्चुएशन को हटाने के लिए
local str_with_punc = "नमस्ते, वर्ल्ड!"
local punc_pattern = "[.,!]"
local cleaned_str = str_with_punc:gsub(punc_pattern, "")
print(cleaned_str) -- नमस्ते वर्ल्ड
```

## Deep Dive (गहराई में जानकारी)
Lua में पैटर्न मैचिंग प्राचीन विचारों पर आधारित है - रेगेक्स (Regular Expressions) से प्रेरणा लेकर। लेकिन Lua के पैटर्न रेगेक्स से सरल होते हैं। `string.gsub` फ़ंक्शन आपको किसी स्ट्रिंग से विशेष पैटर्न्स को हटाने की सुविधा देता है। इसके अलावा, Lua में विकल्प के तौर पर आप `string.gsub` का उपयोग किये बिना लूप्स या इटरेटर्स का प्रयोग भी कर सकते हैं, पर यह अधिक कोड और जटिलता जोड़ देगा। `gsub` न केवल सामान्य कामों के लिए सुविधाजनक है, बल्कि यह अत्यंत शक्तिशाली भी है और कस्टम पैटर्न्स के साथ जटिल परिवर्तन कर सकता है।

## See Also (और भी देखें)
- Lua 5.4 Reference Manual: [Patterns](http://www.lua.org/manual/5.4/manual.html#6.4.1)
- Programming in Lua (Fourth edition): [Strings and Pattern Matching](https://www.lua.org/pil/20.1.html)
