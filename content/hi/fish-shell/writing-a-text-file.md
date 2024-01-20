---
title:                "टेक्स्ट फाइल लिखना"
html_title:           "Bash: टेक्स्ट फाइल लिखना"
simple_title:         "टेक्स्ट फाइल लिखना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/writing-a-text-file.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
टेक्स्ट फाइल बनाना मतलब विशेष सूचना को फाइल में लिखना। प्रोग्रामर लॉग, सेटिंग्स, स्क्रिप्ट्स और डेटा स्टोर करने के लिए यह काम करते हैं।

## कैसे करें:
Fish Shell में टेक्स्ट फाइल लिखने के लिए `echo` कमांड और पाइपिंग (`>`, `>>`) का इस्तेमाल होता है।

```Fish Shell
echo 'नमस्ते दुनिया!' > hello.txt
cat hello.txt
```

सैंपल आउटपुट:

```
नमस्ते दुनिया!
```

एक्जिस्टिंग फाइल में जोड़ने के लिए:

```Fish Shell
echo 'अलविदा दुनिया!' >> hello.txt
cat hello.txt
```

सैंपल आउटपुट:

```
नमस्ते दुनिया!
अलविदा दुनिया!
```

## गहराई से जानकारी
फाइल सिस्टम के शुरुआती विकास से ही टेक्स्ट फाइल का इस्तेमाल हो रहा है। `echo` कमांड और पाइपिंग जैसे बेसिक टूल्स Linux और Unix फिलोसॉफ़ी का हिस्सा हैं जो सिम्पल टूल्स के सॉलिड चेन बनाने पर जोर देता है। 

स्क्रिप्टिंग में विकल्प के तौर पर `sed`, `awk`, या `perl` जैसे प्रोग्राम्स का इस्तेमाल हो सकता है जो पावरफुल टेक्स्ट प्रोसेसिंग प्रदान करते हैं। 

Fish Shell में `>` ऑपरेटर एक नई फाइल बनाता है या एक्जिस्टिंग फाइल को ओवरराइट करता है, जबकि `>>` एक्जिस्टिंग फाइल में डेटा जोड़ता है।

## संबंधित स्रोत
- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [GNU Core Utilities for File Management](https://www.gnu.org/software/coreutils/manual/coreutils.html)
- [Advanced Bash-Scripting Guide](https://tldp.org/LDP/abs/html/)