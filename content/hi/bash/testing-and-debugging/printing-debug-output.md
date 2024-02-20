---
date: 2024-01-20 17:51:56.406104-07:00
description: "Debugging \u0915\u0947 \u0926\u094C\u0930\u093E\u0928 \u092A\u094D\u0930\
  \u094B\u0917\u094D\u0930\u093E\u092E \u0938\u0947 \u0938\u0942\u091A\u0928\u093E\
  \ \u092A\u094D\u0930\u093F\u0902\u091F \u0915\u0930\u0928\u093E \u0939\u094B\u0924\
  \u093E \u0939\u0948 \u091C\u093F\u0938\u0938\u0947 \u092A\u0939\u091A\u093E\u0928\
  \ \u0938\u0915\u0947\u0902 \u0915\u093F \u0915\u094D\u092F\u093E \u091A\u0932 \u0930\
  \u0939\u093E \u0939\u0948\u0964 \u092F\u0939 \u0924\u094D\u0930\u0941\u091F\u093F\
  \u092F\u094B\u0902 \u0915\u0940 \u091C\u093E\u0902\u091A \u0914\u0930 \u0909\u0928\
  \u094D\u0939\u0947\u0902 \u0920\u0940\u0915 \u0915\u0930\u0928\u0947 \u0915\u0947\
  \ \u0932\u093F\u090F \u0915\u093E\u0930\u0917\u0930 \u0939\u0948\u0964"
lastmod: 2024-02-19 22:05:11.649270
model: gpt-4-1106-preview
summary: "Debugging \u0915\u0947 \u0926\u094C\u0930\u093E\u0928 \u092A\u094D\u0930\
  \u094B\u0917\u094D\u0930\u093E\u092E \u0938\u0947 \u0938\u0942\u091A\u0928\u093E\
  \ \u092A\u094D\u0930\u093F\u0902\u091F \u0915\u0930\u0928\u093E \u0939\u094B\u0924\
  \u093E \u0939\u0948 \u091C\u093F\u0938\u0938\u0947 \u092A\u0939\u091A\u093E\u0928\
  \ \u0938\u0915\u0947\u0902 \u0915\u093F \u0915\u094D\u092F\u093E \u091A\u0932 \u0930\
  \u0939\u093E \u0939\u0948\u0964 \u092F\u0939 \u0924\u094D\u0930\u0941\u091F\u093F\
  \u092F\u094B\u0902 \u0915\u0940 \u091C\u093E\u0902\u091A \u0914\u0930 \u0909\u0928\
  \u094D\u0939\u0947\u0902 \u0920\u0940\u0915 \u0915\u0930\u0928\u0947 \u0915\u0947\
  \ \u0932\u093F\u090F \u0915\u093E\u0930\u0917\u0930 \u0939\u0948\u0964"
title: "\u0921\u0940\u092C\u0917 \u0906\u0909\u091F\u092A\u0941\u091F \u092A\u094D\
  \u0930\u093F\u0902\u091F \u0915\u0930\u0928\u093E"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
Debugging के दौरान प्रोग्राम से सूचना प्रिंट करना होता है जिससे पहचान सकें कि क्या चल रहा है। यह त्रुटियों की जांच और उन्हें ठीक करने के लिए कारगर है।

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
