---
date: 2024-01-20 17:45:37.296694-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Fish Shell\
  \ \u092E\u0947\u0902 \u0938\u092C\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\
  \ \u0928\u093F\u0915\u093E\u0932\u0928\u0947 \u0915\u0947 \u0909\u0926\u093E\u0939\
  \u0930\u0923."
lastmod: '2024-03-13T22:44:53.043555-06:00'
model: gpt-4-1106-preview
summary: "Fish Shell \u092E\u0947\u0902 \u0938\u092C\u0938\u094D\u091F\u094D\u0930\
  \u093F\u0902\u0917 \u0928\u093F\u0915\u093E\u0932\u0928\u0947 \u0915\u0947 \u0909\
  \u0926\u093E\u0939\u0930\u0923."
title: "\u0938\u092C\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0928\
  \u093F\u0915\u093E\u0932\u0928\u093E"
weight: 6
---

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
