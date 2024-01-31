---
title:                "परीक्षण लिखना"
date:                  2024-01-19
html_title:           "Arduino: परीक्षण लिखना"
simple_title:         "परीक्षण लिखना"

category:             "Bash"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
टेस्ट लेखन से हमारा मतलब कोड के लिए चेक करना है कि वह ठीक से चल रहा है या नहीं। प्रोग्रामर्स इसे इसलिए करते हैं, ताकि बग्स कम हों और कोड की गुणवत्ता बढ़े।

## How to: (कैसे करें:)
Bash स्क्रिप्ट में टेस्ट करने के लिए आप `[` और `test` कमांड का इस्तेमाल कर सकते हैं। नीचे एक सामान्य उदाहरण दिया गया है:

```Bash
#!/bin/bash

# स्ट्रिंग टेस्ट
if [ "$1" == "hello" ]; then
  echo "Test Passed: Argument hello है।"
else
  echo "Test Failed: Argument hello नहीं है।"
fi
```

इस स्क्रिप्ट का उपयोग करते समय अगर आप `hello` आर्ग्यूमेंट देते हैं, आउटपुट होगा:

```
Test Passed: Argument hello है।
```

## Deep Dive (गहराई से जानकारी)
Bash में टेस्ट लेखन का प्रचलन 1970 के दशक में शुरू हुआ जब Unix परवान चढ़ रहा था। `[` कमांड वास्तव में `test` कमांड का एक ही रूप है जो कंडीशनल एक्सप्रेशन का परीक्षण करता है। बदलते समय के साथ इसके अल्टरनेटिव्स भी आ गए हैं, जैसे `[[` जो ज्यादा क्षमतावान है और बेहतर विशेषताएं प्रदान करता है। इन टूल्स की मदद से डेवलपर्स फ़ाइलों, स्ट्रिंग्स और नंबर्स की तुलना कर सकते हैं।

## See Also (और भी देखें)
- Bash मैन्युअल पेज: https://www.gnu.org/software/bash/manual/
- Advanced Bash Scripting Guide: https://www.tldp.org/LDP/abs/html/
- Google's Shell Style Guide: https://google.github.io/styleguide/shellguide.html
- Bash Test Constructs: https://wiki.bash-hackers.org/syntax/ccmd/conditional_expression
