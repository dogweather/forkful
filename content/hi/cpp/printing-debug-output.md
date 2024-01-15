---
title:                "डिबग आउटपुट को प्रिंट करना"
html_title:           "C++: डिबग आउटपुट को प्रिंट करना"
simple_title:         "डिबग आउटपुट को प्रिंट करना"
programming_language: "C++"
category:             "C++"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/cpp/printing-debug-output.md"
---

{{< edit_this_page >}}

## क्यों

कभी-कभी हमारे कोड में बग हो जाते हैं और हमें उन्हें ढूंढ़ने और सुधारने के लिए विस्तारित लोग लिखने की आवश्यकता होती है। जब हम धीमे प्रगति को देखते हैं, तो उन्हें ढूंढ़ने और ठीक करने को अधिक सुविधाजनक बनाने के लिए हमारे द्वारा लिखे गए कोड में डीबग आउटपुट प्रिंट करना अत्यंत उपयोगी हो सकता है। 

## कैसे

```
// Define a macro for printing debug output
#define DEBUG(x) cout << "Debug: " << x << endl;

// Example of its usage
int num = 5;
DEBUG("Value of num is " << num);

// Output:
// Debug: Value of num is 5
```

ऊपर दिए गए कोड की मदद से हम एक DEBUG() मैक्रो बना सकते हैं जो किसी भी अभिव्यक्ति को प्रिंट करेगा और एक लाइन खाली छोड़ेगा। हम वहां कोई भी मान या अभिव्यक्ति दे सकते हैं जो हमें संदेश के साथ प्रिंट करना हो।

## गहराई में

डीबग आउटपुट प्रिंट करने का अधिकतर सबसे आसान तरीका सीआरटी मैक्रो का उपयोग करना होता है। यह हमारे कोड को बग-मुक्त बनाने के लिए एक महत्वपूर्ण उपकरण हो सकता है। इसके साथ हम अपने कोड को आसानी से दिखा सकते हैं और समस्याओं का पता लगा सकते हैं जो हमारे द्वारा कैसे संशोधित किए जा सकते हैं। 

## देखें भी

- [सी++ के मैक्रो](https://www.geeksforgeeks.org/macros-preprocessor-cc/)
- [डीबगिंग ट्रिक्स](https://medium.com/coderbyte/top-7-ways-to-debug-your-code-d9b60d461519)