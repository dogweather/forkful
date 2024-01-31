---
title:                "पैटर्न से मेल खाते अक्षरों को हटाना"
date:                  2024-01-20T17:41:47.754761-07:00
model:                 gpt-4-1106-preview
simple_title:         "पैटर्न से मेल खाते अक्षरों को हटाना"

category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
पैटर्न से मिलते अक्षरों को हटाना मतलब किसी टेक्स्ट स्ट्रिंग में से खास पैटर्न के अनुरूप अक्षरों का चयन और उन्हें हटाना। प्रोग्रामर यह करते हैं डेटा को साफ़ करने, आवश्यक जानकारी निकालने या टेक्स्ट प्रोसेसिंग के लिए।

## How to (कैसे करें):
```Bash
# पैटर्न से मिलते अक्षर हटाने के लिए tr कमांड का इस्तेमाल
echo "Hello World 123" | tr -d '1-9'
# आउटपुट: Hello World

# स्पेसिफिक शब्द हटाने के लिए sed कमांड
echo "Bash Programming is fun" | sed 's/fun//'
# आउटपुट: Bash Programming is

# पूरी लाइनों को हटाना grep के -v ऑप्शन से
echo -e "Welcome\nProgramming in Bash\nGoodbye" | grep -v "Programming"
# आउटपुट:
# Welcome
# Goodbye
```

## Deep Dive (गहराई में जानकारी):
पैटर्न मैचिंग के लिए यूनिक्स में ज्यादातर `sed`, `awk`, `grep`, और `tr` जैसे प्रोग्राम्स का इस्तेमाल होता है। इनका अस्तित्व 1970 के दशक की शुरुआत में ही हो गया था जब यूनिक्स ऑपरेटिंग सिस्टम विकसित किया जा रहा था। `tr` टेक्स्ट रिप्लेसमेंट के लिए प्राचीन लेकिन सरल उपकरण है, जो कैरेक्टर-बाय-कैरेक्टर प्रोसेसिंग करता है। `sed` (Stream Editor) पाइपलाइन में डेटा स्ट्रीम्स पर काम करता है और सबसे विश्वसनीय एडिटर्स में से एक है। `grep` पैटर्न को ढूंढने और छानने में सक्षम है, जबकि `awk` टेक्स्ट पर प्रतिरूपण और डेटा मैनिपुलेशन के लिए एक प्रोग्रामिंग भाषा है।

## See Also (और भी देखें):
- GNU Coreutils Manual for `tr`: https://www.gnu.org/software/coreutils/manual/html_node/tr-invocation.html
- GNU Sed Manual: https://www.gnu.org/software/sed/manual/sed.html
- Grep Manual Page: https://man7.org/linux/man-pages/man1/grep.1.html
- Awk Manual Page: https://man7.org/linux/man-pages/man1/awk.1p.html
