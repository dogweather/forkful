---
date: 2024-01-20 17:52:44.419154-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Fish Shell\
  \ \u092E\u0947\u0902 \u0921\u0940\u092C\u0917 \u0906\u0909\u091F\u092A\u0941\u091F\
  \ \u0915\u094B \u092A\u094D\u0930\u093F\u0902\u091F \u0915\u0930\u0928\u093E \u0906\
  \u0938\u093E\u0928 \u0939\u0948\u0964 \u092F\u0939\u093E\u0902 \u090F\u0915 \u0909\
  \u0926\u093E\u0939\u0930\u0923 \u0926\u093F\u092F\u093E \u0917\u092F\u093E \u0939\
  \u0948."
lastmod: '2024-03-13T22:44:53.070771-06:00'
model: gpt-4-1106-preview
summary: "Fish Shell \u092E\u0947\u0902 \u0921\u0940\u092C\u0917 \u0906\u0909\u091F\
  \u092A\u0941\u091F \u0915\u094B \u092A\u094D\u0930\u093F\u0902\u091F \u0915\u0930\
  \u0928\u093E \u0906\u0938\u093E\u0928 \u0939\u0948\u0964 \u092F\u0939\u093E\u0902\
  \ \u090F\u0915 \u0909\u0926\u093E\u0939\u0930\u0923 \u0926\u093F\u092F\u093E \u0917\
  \u092F\u093E \u0939\u0948."
title: "\u0921\u0940\u092C\u0917 \u0906\u0909\u091F\u092A\u0941\u091F \u092A\u094D\
  \u0930\u093F\u0902\u091F \u0915\u0930\u0928\u093E"
weight: 33
---

## How to: (कैसे करें:)
Fish Shell में डीबग आउटपुट को प्रिंट करना आसान है। यहां एक उदाहरण दिया गया है:

```Fish Shell
function my_debug_function
    set -l message $argv
    echo "Debug: $message"
end

my_debug_function "नमस्ते, दुनिया!"
```
उदाहरण के आउटपुट:
```
Debug: नमस्ते, दुनिया!
```

## Deep Dive (गहराई में जानकारी):
डीबग आउटपुट प्रिंटिंग की परंपरा कंप्यूटर प्रोग्रामिंग के शुरुआती दिनों से चली आ रही है। Fish Shell के प्रतिद्वंद्वियों, जैसे की bash और zsh, में भी इसी तरह की सुविधाएँ होती हैं, लेकिन Fish अपने सरल सिंटैक्स और user-friendly डिजाइन के लिए जाना जाता है। डीबग आउटपुट को फिश में प्रिंट करते समय, आप stderr का उपयोग करके या `status --is-interactive` कमांड का उपयोग करके अधिक गहराई में जा सकते हैं।

## See Also (यह भी देखें):
- Fish Shell ऑफिसियल डाक्यूमेंटेशन: https://fishshell.com/docs/current/index.html
- Stack Overflow: पर Fish Shell से संबंधित सामान्य प्रश्न और उत्तर https://stackoverflow.com/questions/tagged/fish
