---
title:                "डीबग आउटपुट प्रिंट करना"
html_title:           "Gleam: डीबग आउटपुट प्रिंट करना"
simple_title:         "डीबग आउटपुट प्रिंट करना"
programming_language: "Python"
category:             "Python"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/printing-debug-output.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

डीबग आउटपुट प्रिंट करना मतलब किसी प्रोग्राम में मौजूद बग्स को खोजने के लिए आउटपुट को देखना। यह प्रोग्रामर्स को बग्स को दूर करने में मदद करता है और किसी प्रोग्राम को समझने में भी। 

## कैसे :

```Python
# साधारण print का उपयोग करके
print("This is debug output")

# logging module का उपयोग करें।
import logging
logging.debug("This is debug output")

# आउटपुट:
# This is debug output
# DEBUG:root:This is debug output
```

## गहरी डाइव:

प्राचीन समय से ही प्रोग्रामर्स प्रिंट डीबगिंग का उपयोग कर रहे हैं। लेकिन, हाल ही में logging module का उपयोग हो रहा है क्योंकि इसे लॉग की स्तर को control करने की अनुमति देता है। इसके विकल्प में pdb (Python Debugger) और pdb++ जैसे उपलब्ध हैं। 

## देखने के लिये:

1. [Python's official debugging documentation](https://docs.python.org/3/library/debug.html)