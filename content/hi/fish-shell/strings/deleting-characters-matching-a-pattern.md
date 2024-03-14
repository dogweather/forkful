---
date: 2024-01-20 17:42:36.129656-07:00
description: "\u092A\u0948\u091F\u0930\u094D\u0928 \u0938\u0947 \u092E\u0947\u0932\
  \ \u0916\u093E\u0928\u0947 \u0935\u093E\u0932\u0947 \u0905\u0915\u094D\u0937\u0930\
  \ \u0939\u091F\u093E\u0928\u093E \u092E\u0924\u0932\u092C \u0939\u0948 \u0915\u0941\
  \u091B \u0928\u093F\u0936\u094D\u091A\u093F\u0924 \u0928\u093F\u092F\u092E\u094B\
  \u0902 \u0915\u0947 \u0906\u0927\u093E\u0930 \u092A\u0930 \u091F\u0947\u0915\u094D\
  \u0938\u094D\u091F \u0938\u0947 \u0916\u093E\u0938 \u0905\u0915\u094D\u0937\u0930\
  \u094B\u0902 \u0915\u094B \u0928\u093F\u0915\u093E\u0932 \u0926\u0947\u0928\u093E\
  \u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938\
  \ \u092F\u0939 \u0915\u093E\u092E \u0921\u0947\u091F\u093E \u0915\u094B \u0938\u093E\
  \u092B-\u0938\u0941\u0925\u0930\u093E \u0914\u0930\u2026"
lastmod: '2024-03-13T22:44:53.035265-06:00'
model: gpt-4-1106-preview
summary: "\u092A\u0948\u091F\u0930\u094D\u0928 \u0938\u0947 \u092E\u0947\u0932 \u0916\
  \u093E\u0928\u0947 \u0935\u093E\u0932\u0947 \u0905\u0915\u094D\u0937\u0930 \u0939\
  \u091F\u093E\u0928\u093E \u092E\u0924\u0932\u092C \u0939\u0948 \u0915\u0941\u091B\
  \ \u0928\u093F\u0936\u094D\u091A\u093F\u0924 \u0928\u093F\u092F\u092E\u094B\u0902\
  \ \u0915\u0947 \u0906\u0927\u093E\u0930 \u092A\u0930 \u091F\u0947\u0915\u094D\u0938\
  \u094D\u091F \u0938\u0947 \u0916\u093E\u0938 \u0905\u0915\u094D\u0937\u0930\u094B\
  \u0902 \u0915\u094B \u0928\u093F\u0915\u093E\u0932 \u0926\u0947\u0928\u093E\u0964\
  \ \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u092F\
  \u0939 \u0915\u093E\u092E \u0921\u0947\u091F\u093E \u0915\u094B \u0938\u093E\u092B\
  -\u0938\u0941\u0925\u0930\u093E \u0914\u0930\u2026"
title: "\u092A\u0948\u091F\u0930\u094D\u0928 \u0938\u0947 \u092E\u0947\u0932 \u0916\
  \u093E\u0924\u0947 \u0905\u0915\u094D\u0937\u0930\u094B\u0902 \u0915\u094B \u0939\
  \u091F\u093E\u0928\u093E"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
पैटर्न से मेल खाने वाले अक्षर हटाना मतलब है कुछ निश्चित नियमों के आधार पर टेक्स्ट से खास अक्षरों को निकाल देना। प्रोग्रामर्स यह काम डेटा को साफ-सुथरा और प्रोसेसिंग के लिए तैयार करने हेतु करते हैं।

## How to: (कैसे करें:)
```Fish Shell
# एक स्ट्रिंग से सभी अंक हटाने का उदाहरण
echo "Fish123Shell456" | string replace -ar "[0-9]" ""
```
```
FishShell
```

```Fish Shell
# फ़ाइल के नाम से विशेष वर्णों को हटाना
for file in *
    mv $file (echo $file | string replace -ar "[@#$%^&*()]" "")
end
```
उपरोक्त कोड फ़ाइल्स के नाम से `@#$%^&*()` इन विशेष वर्णों को हटा देगा।

## Deep Dive (गहराई में जानकारी)
Fish Shell में `string` कमांड टेक्स्ट प्रोसेसिंग के लिए सबसे उपयोगी टूल्स में से एक है। 'string replace' का उपयोग कर कोई भी पैटर्न डिलीट करना आसान है। पहले लोग sed और awk जैसे टूल्स का उपयोग करते थे, लेकिन Fish में बिल्ट-इन `string` कमांड की वजह से यह काम बहुत सटीक और सहज हो गया है। 'string replace -ar' का मतलब है 'all occurrences' (सभी उदाहरणों को) 'regular expression' (रेगुलर एक्सप्रेशन के द्वारा) से रिप्लेस करना।

## See Also (और जानकारी के लिए)
- Fish Shell Documentation: [fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- String Manipulation with Fish: [fishshell.com/docs/current/cmds/string.html](https://fishshell.com/docs/current/cmds/string.html)
- Regular Expressions Introduction: [regular-expressions.info](https://www.regular-expressions.info)
