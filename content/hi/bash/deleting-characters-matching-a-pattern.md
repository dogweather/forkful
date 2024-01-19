---
title:                "पैटर्न से मिलते जुलते वर्णों को हटाना"
html_title:           "Elixir: पैटर्न से मिलते जुलते वर्णों को हटाना"
simple_title:         "पैटर्न से मिलते जुलते वर्णों को हटाना"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

एक pattern से मेल खाने वाले किरदारों को हटाना क्या होता है? यह एक प्रोग्रामिंग क्रिया है जिसमें किसी विशेष पैटर्न के साथ मेल खाने वाले किरदारों को कोड से हटाया जाता है। प्रोग्रामर्स इसे क्यों करते हैं? क्योंकि इससे उन्हें किरदारों के अनावश्यक सेट को बाहर निकालने और स्पष्ट, क्रियाशील कोड लिखने में मदद मिलती है।

## कैसे करें:

मेल खाने वाले किरदारों को हटाने के लिए, आप `tr -d` कमांड का उपयोग कर सकते हैं। 

```Bash
echo "नमस्ते दुनिया!" | tr -d 'न'
# Output: मस्ते दुिया!
```

ऊपर के कोड का कार्य क्या है? यह 'न' किरदार को "नमस्ते दुनिया!" स्ट्रिंग से हटा देता है। 

## गहराई में जानने के लिए

आपने देखा कि 'tr' कमांड कैसे किरदारों को हटाती है। लेकिन इसके पीछे का इतिहास क्या है? `tr` कमांड Unix परिवार के कंप्यूटर ऑपरेटिंग सिस्टम का हिस्सा है और यह ट्रांसपोज़ किरदार की मदद करता है। 

वैकल्पिक रूप से, आप `sed` कमांड का भी उपयोग कर सकते हैं। 

```Bash
echo "नमस्ते दुनिया!" | sed 's/न//g'
# Output: मस्ते दुया!
```

## अधिक जानकारी देखिए 

1. [GNU Bash Reference Manual - The GNU Operating System and the Free Software Foundation (FSF)](https://www.gnu.org/software/bash/manual/bash.html)
2. [Using the TR command](https://shapeshed.com/unix-tr/)
3. [Bash String Manipulation](http://tldp.org/LDP/abs/html/string-manipulation.html)