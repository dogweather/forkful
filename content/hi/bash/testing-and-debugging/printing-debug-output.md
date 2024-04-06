---
date: 2024-01-20 17:51:56.406104-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902) Debugging\
  \ output \u0915\u093E \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 1970s \u0938\
  \u0947 \u0939\u094B \u0930\u0939\u093E \u0939\u0948, \u091C\u092C \u0938\u0949\u092B\
  \u094D\u091F\u0935\u0947\u092F\u0930 \u0915\u0940 \u091C\u091F\u093F\u0932\u0924\
  \u093E \u092C\u0922\u093C\u0928\u0947 \u0932\u0917\u0940\u0964 Bash \u092E\u0947\
  \u0902 `echo` \u0914\u0930 `printf` \u091C\u0948\u0938\u0947 \u0915\u092E\u093E\u0902\
  \u0921\u094D\u0938 \u0915\u093E \u0909\u092A\u092F\u094B\u0917\u2026"
lastmod: '2024-04-05T22:51:07.305636-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902) Debugging output \u0915\
  \u093E \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 1970s \u0938\u0947 \u0939\
  \u094B \u0930\u0939\u093E \u0939\u0948, \u091C\u092C \u0938\u0949\u092B\u094D\u091F\
  \u0935\u0947\u092F\u0930 \u0915\u0940 \u091C\u091F\u093F\u0932\u0924\u093E \u092C\
  \u0922\u093C\u0928\u0947 \u0932\u0917\u0940\u0964 Bash \u092E\u0947\u0902 `echo`\
  \ \u0914\u0930 `printf` \u091C\u0948\u0938\u0947 \u0915\u092E\u093E\u0902\u0921\u094D\
  \u0938 \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0939\u094B\u0924\u093E \u0939\
  \u0948\u0964 \u0905\u0932\u0917-\u0905\u0932\u0917 \u0932\u0947\u0935\u0932 \u0915\
  \u0947 \u0932\u093F\u090F `set -x`, `trap`, \u0914\u0930 functions \u0915\u093E\
  \ \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\u0947 \u091C\u094D\u092F\u093E\
  \u0926\u093E granular debugging \u0938\u0947\u091F \u0915\u093F\u092F\u093E \u091C\
  \u093E \u0938\u0915\u0924\u093E \u0939\u0948\u0964 \u092C\u0947\u0939\u0924\u0930\
  \ \u090F\u0915\u094D\u0938\u092A\u0940\u0930\u093F\u092F\u0902\u0938 \u0915\u0947\
  \ \u0932\u093F\u090F log files \u092E\u0947\u0902 \u0921\u0940\u092C\u0917 \u0907\
  \u0902\u092B\u0949\u0930\u094D\u092E\u0947\u0936\u0928 \u0932\u093F\u0916\u0928\u093E\
  \ \u0914\u0930 external \u091F\u0942\u0932\u094D\u0938 \u091C\u0948\u0938\u0947\
  \ ki `bashdb` \u0915\u093E \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0939\
  \u094B\u0924\u093E \u0939\u0948\u0964."
title: "\u0921\u0940\u092C\u0917 \u0906\u0909\u091F\u092A\u0941\u091F \u092A\u094D\
  \u0930\u093F\u0902\u091F \u0915\u0930\u0928\u093E"
weight: 33
---

## How to: (कैसे करें)
```Bash
# साधारण प्रिंट उदाहरण
echo "Hello, Debugging World!"

# वेरिएबल के मान के साथ
VAR="Debugging value"
echo "Variable value: $VAR"

# कंडिशनल डीबग मैसेज
DEBUG=true
if [ "$DEBUG" = true ]; then
  echo "Debug Mode is ON."
fi
```
सैम्पल आउटपुट:
```
Hello, Debugging World!
Variable value: Debugging value
Debug Mode is ON.
```

## Deep Dive (गहराई से समझें)
Debugging output का इस्तेमाल 1970s से हो रहा है, जब सॉफ्टवेयर की जटिलता बढ़ने लगी। Bash में `echo` और `printf` जैसे कमांड्स का उपयोग होता है। अलग-अलग लेवल के लिए `set -x`, `trap`, और functions का उपयोग करके ज्यादा granular debugging सेट किया जा सकता है। बेहतर एक्सपीरियंस के लिए log files में डीबग इंफॉर्मेशन लिखना और external टूल्स जैसे ki `bashdb` का इस्तेमाल होता है।

## See Also (इसे भी देखें)
- Bash Manual: https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html
- Advanced Bash-Scripting Guide: http://tldp.org/LDP/abs/html/debugging.html
- Stack Overflow Debugging Techniques: https://stackoverflow.com/questions/tagged/bash+debugging
