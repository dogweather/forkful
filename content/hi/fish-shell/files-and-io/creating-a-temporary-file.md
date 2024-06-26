---
date: 2024-01-20 17:41:05.341175-07:00
description: "How to (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902): Fish Shell\
  \ \u092E\u0947\u0902 temporary file \u092C\u0928\u093E\u0928\u093E \u092C\u0939\u0941\
  \u0924 \u0938\u0940\u0927\u093E \u0939\u0948. \u0928\u0940\u091A\u0947 \u0926\u094B\
  \ \u0924\u0930\u0940\u0915\u093C\u0947 \u0926\u093F\u090F \u0917\u090F \u0939\u0948\
  \u0902."
lastmod: '2024-03-13T22:44:53.099980-06:00'
model: gpt-4-1106-preview
summary: "Fish Shell \u092E\u0947\u0902 temporary file \u092C\u0928\u093E\u0928\u093E\
  \ \u092C\u0939\u0941\u0924 \u0938\u0940\u0927\u093E \u0939\u0948."
title: "\u0905\u0938\u094D\u0925\u093E\u092F\u0940 \u092B\u093E\u0907\u0932 \u092C\
  \u0928\u093E\u0928\u093E"
weight: 21
---

## How to (कैसे करें):
Fish Shell में temporary file बनाना बहुत सीधा है. नीचे दो तरीक़े दिए गए हैं:

```Fish Shell
# तरीक़ा 1: mktemp कमांड का इस्तेमाल करना
set tempfile (mktemp)
echo "This is a temporary file" > $tempfile
cat $tempfile
# Output:
# This is a temporary file

# फाइल डिलीट कर दें
rm $tempfile

# तरीक़ा 2: Piping और Process Substitution का यूज़ करना
echo "Store this data temporarily" | read -P tempfile
cat $tempfile
# Output:
# Store this data temporarily
```

## Deep Dive (गहराई में):
Temporary files की अवधारणा बहुत पुरानी है, Unix-like systems में यह काफ़ी आम है. `/tmp` डायरेक्टरी अस्थायी फाइलों के लिए समर्पित होती है. 

Fish Shell में `mktemp` कमांड एक यूनीक फाइल नेम बनाता है, जिसे temporary data होल्ड करने के लिए इस्तेमाल किया जा सकता है. यह एक सुरक्षित तरीक़ा है, क्योंकि इससे फाइल नेम clashes से बचा जा सकता है.

Alternatives में `tempfile` कमांड भी शामिल है, जो कुछ लिनक्स डिस्ट्रीब्यूशंस पर उपलब्ध होता है. 

Implementation details की बात करें तो, `mktemp` फाइल नेम में रैंडम शब्दों का उपयोग करता है, जो collision और security risks को कम करता है.

## See Also (और देखें):
- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [Unix mktemp(1) man page](https://man7.org/linux/man-pages/man1/mktemp.1.html)
- [Advanced Bash-Scripting Guide: Temporary Files](https://tldp.org/LDP/abs/html/tempfiles.html)
