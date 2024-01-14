---
title:    "Bash: डिबग आउटपुट छपाई"
keywords: ["Bash"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/bash/printing-debug-output.md"
---

{{< edit_this_page >}}

## क्यों
प्रोग्रामिंग में डिबग आउटपुट प्रिंट करने का कारण आपको अपने कोड में गड़बड़ी को पहचानने और सुधार करने में मदद करना है।

## कैसे करें
आप अपने बैश स्क्रिप्ट में `echo` या `printf` कमांड का उपयोग करके डिबग आउटपुट को प्रिंट कर सकते हैं। इसके अलावा, आप `set -x` कमांड का भी उपयोग कर सकते हैं जो डिबग मोड में स्क्रिप्ट के सभी पंक्तियों को प्रिंट करता है।

इस उदाहरण में, हम `echo` कमांड का उपयोग करके एक संदेश प्रिंट करते हैं:

```Bash
echo "मेरा संदेश"
```

यहां इसका आउटपुट होगा:

```
मेरा संदेश
```

इससे आप अपने कोड रन करने के दौरान अपने संदेश को ट्रैक कर सकते हैं और किसी भी गलती को पहचानने में मदद मिलेगी।

## गहराई से जाने
डिबग आउटपुट प्रिंट करने से आप अपने कोड के साथ खिलवाड़ खत्म कर सकते हैं और सही समय पर समस्याओं को पहचान सकते हैं। जब आप प्रोग्रामिंग से नए होते हैं तो यह बात खासी जरूरी हो सकती है क्योंकि आपके कोड में बग को ढूंढना मुश्किल होता है।

## इन वेबसाइटों पर अधिक जानकारी के लिए देखें
- [Bash scripting tutorial in Hindi](https://www.geeksforgeeks.org/bash-scripting-tutorial/)
- [Debugging techniques in Bash](https://www.cyberciti.biz/tips/debugging-shell-script.html)
- [Advanced Bash debugging guide](https://wiki.bash-hackers.org/scripting/debuggingtips)

## देखें भी
[Markdown कुशलताएं](https://github.com/adam-p/markdown-here/wiki/Markdown-Cheatsheet)