---
title:                "डीबग आउटपुट प्रिंट करना"
date:                  2024-01-20T17:51:56.406104-07:00
model:                 gpt-4-1106-preview
simple_title:         "डीबग आउटपुट प्रिंट करना"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/printing-debug-output.md"
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
