---
title:                "सबस्ट्रिंग्स निकालना"
aliases: - /hi/fish-shell/extracting-substrings.md
date:                  2024-01-20T17:45:37.296694-07:00
model:                 gpt-4-1106-preview
simple_title:         "सबस्ट्रिंग्स निकालना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

सबस्ट्रिंग एक्सट्रैक्शन यानी मुख्य स्ट्रिंग से छोटे टुकड़े को निकालना। प्रोग्रामर लॉग्स पढ़ने, डेटा पर्सिंग या कंटेंट में से खास जानकारी निकालने के लिए यह करते हैं।

## How to: (कैसे करें:)

Fish Shell में सबस्ट्रिंग निकालने के उदाहरण:

```Fish Shell
# मान लो $txt एक स्ट्रिंग वेरिएबल है जिसमें 'नमस्ते दुनिया' स्टोर है
set txt "नमस्ते दुनिया"

# पहले 6 कैरेक्टर्स निकालें
echo $txt[1..6]
# Output: नमस्ते

# 8वें कैरेक्टर से आखिर तक निकालें
echo $txt[8..-1]
# Output: दुनिया
```

## Deep Dive (गहराई में जानकारी):

सबस्ट्रिंग एक्सट्रैक्शन की कुछ इतिहास और विस्तार से जानकारी:

1. इतिहास: स्ट्रिंग मैनिपुलेशन हमेशा से प्रोग्रामिंग का हिस्सा रहा है और समय के साथ इसमें लगातार सुधार हुआ है।

2. विकल्प: `awk`, `sed`, `cut` जैसे कमांड्स का इस्तेमाल करके भी UNIX शेल्स में सबस्ट्रिंग पाया जा सकता है। 

3. लागू करने की डिटेल्स: Fish Shell तरीके बताता है कि कैसे सिंपल इंडेक्स सिंटेक्स का इस्तेमाल करके आसानी से और इफेक्टिवली सबस्ट्रिंग्स निकाले जा सकते हैं।

## See Also (इसे भी देखें):

1. Fish Shell Documentation: [https://fishshell.com/docs/current/index.html#syntax](https://fishshell.com/docs/current/index.html#syntax)
2. String Manipulation in Fish Shell: [https://fishshell.com/docs/current/cmds/string.html](https://fishshell.com/docs/current/cmds/string.html)
3. Community Tutorials and Guides: [https://github.com/fish-shell/fish-shell/wiki/Tutorial](https://github.com/fish-shell/fish-shell/wiki/Tutorial)
