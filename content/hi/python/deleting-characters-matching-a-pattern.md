---
title:                "पैटर्न से मेल खाते अक्षरों को हटाना"
date:                  2024-01-20T17:43:48.982368-07:00
model:                 gpt-4-1106-preview
simple_title:         "पैटर्न से मेल खाते अक्षरों को हटाना"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
पैटर्न के अनुरूप अक्षर हटाना यह है कि खास पैटर्न से मिलते जुलते अक्षरों को स्ट्रिंग से निकाल देना। प्रोग्रामर्स इसे इसलिए करते हैं क्योंकि कई बार हमें सिर्फ जरुरी डेटा चाहिए होता है, बेकार के अक्षरों या शब्दों को हटाकर साफ-सुथरा डेटा पाने के लिए।

## तरीका (How to:)
```Python
import re

# उदाहरण स्ट्रिंग
example_string = "Hello123, नमस्ते456, Bye789"

# पैटर्न: अंक हटाओ
pattern = '[0-9]'

# उस पैटर्न को मिलाने वाले चरित्रों को हटाएं
cleaned_string = re.sub(pattern, '', example_string)

print(cleaned_string)  # 'Hello, नमस्ते, Bye'
```

## डीप डाइव (Deep Dive)
पैटर्न मिलानकर्ता, जैसे की रेगुलर एक्सप्रेशन्स (regular expressions), शक्तिशाली उपकरण हैं जो 1950 के दशक से उपयोग में हैं। Python में `re` मॉड्यूल का उपयोग इन पैटर्न मिलानकर्ता के लिए किया जाता है। `re.sub()` फंक्शन आपको इस पैटर्न वाले अक्षरों को हटा के स्ट्रिंग को साफ करने देता है।

एक और तरीका `str.translate()` हो सकता है, पर यह बहुत जटिल पैटर्न के लिए इतना अच्छा नहीं है। `re.sub()` आपको पॉवरफुल पैटर्न मैचिंग क्षमता देता है वो भी जटिलता को समझने में आसान तरीके से।

जब आप `re.sub()` का उपयोग करते हैं, आपको ध्यान देना होता है कि पैटर्न सही से लिखा जाए क्योंकि गलत पैटर्न का मतलब होगा गलत डेटा। इसलिए, जब भी पैटर्न लिखें, उसे टेस्ट करना ना भूलें।

## यह भी देखें (See Also)
- Python `re` मॉड्यूल का दस्तावेज: [Python re documentation](https://docs.python.org/3/library/re.html)
- रेगुलर एक्सप्रेशन्स क्विक स्टार्ट गाइड: [Regular Expressions Quick Start](https://www.regular-expressions.info/quickstart.html)
- `re.sub()` के और उदाहरणों के लिए: [re.sub() examples](https://www.programiz.com/python-programming/methods/string/replace)
