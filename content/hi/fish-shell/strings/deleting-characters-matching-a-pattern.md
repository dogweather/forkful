---
date: 2024-01-20 17:42:36.129656-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Fish Shell\
  \ \u092E\u0947\u0902 `string` \u0915\u092E\u093E\u0902\u0921 \u091F\u0947\u0915\u094D\
  \u0938\u094D\u091F \u092A\u094D\u0930\u094B\u0938\u0947\u0938\u093F\u0902\u0917\
  \ \u0915\u0947 \u0932\u093F\u090F \u0938\u092C\u0938\u0947 \u0909\u092A\u092F\u094B\
  \u0917\u0940 \u091F\u0942\u0932\u094D\u0938 \u092E\u0947\u0902 \u0938\u0947 \u090F\
  \u0915 \u0939\u0948\u0964 'string replace' \u0915\u093E \u0909\u092A\u092F\u094B\
  \u0917 \u0915\u0930 \u0915\u094B\u0908 \u092D\u0940 \u092A\u0948\u091F\u0930\u094D\
  \u0928\u2026"
lastmod: '2024-04-05T22:51:07.697266-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Fish Shell \u092E\u0947\
  \u0902 `string` \u0915\u092E\u093E\u0902\u0921 \u091F\u0947\u0915\u094D\u0938\u094D\
  \u091F \u092A\u094D\u0930\u094B\u0938\u0947\u0938\u093F\u0902\u0917 \u0915\u0947\
  \ \u0932\u093F\u090F \u0938\u092C\u0938\u0947 \u0909\u092A\u092F\u094B\u0917\u0940\
  \ \u091F\u0942\u0932\u094D\u0938 \u092E\u0947\u0902 \u0938\u0947 \u090F\u0915 \u0939\
  \u0948\u0964 'string replace' \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\
  \u0930 \u0915\u094B\u0908 \u092D\u0940 \u092A\u0948\u091F\u0930\u094D\u0928 \u0921\
  \u093F\u0932\u0940\u091F \u0915\u0930\u0928\u093E \u0906\u0938\u093E\u0928 \u0939\
  \u0948\u0964 \u092A\u0939\u0932\u0947 \u0932\u094B\u0917 sed \u0914\u0930 awk \u091C\
  \u0948\u0938\u0947 \u091F\u0942\u0932\u094D\u0938 \u0915\u093E \u0909\u092A\u092F\
  \u094B\u0917 \u0915\u0930\u0924\u0947 \u0925\u0947, \u0932\u0947\u0915\u093F\u0928\
  \ Fish \u092E\u0947\u0902 \u092C\u093F\u0932\u094D\u091F-\u0907\u0928 `string` \u0915\
  \u092E\u093E\u0902\u0921 \u0915\u0940 \u0935\u091C\u0939 \u0938\u0947 \u092F\u0939\
  \ \u0915\u093E\u092E \u092C\u0939\u0941\u0924 \u0938\u091F\u0940\u0915 \u0914\u0930\
  \ \u0938\u0939\u091C \u0939\u094B \u0917\u092F\u093E \u0939\u0948\u0964 'string\
  \ replace -ar' \u0915\u093E \u092E\u0924\u0932\u092C \u0939\u0948 'all occurrences'\
  \ (\u0938\u092D\u0940 \u0909\u0926\u093E\u0939\u0930\u0923\u094B\u0902 \u0915\u094B\
  ) 'regular expression' (\u0930\u0947\u0917\u0941\u0932\u0930 \u090F\u0915\u094D\u0938\
  \u092A\u094D\u0930\u0947\u0936\u0928 \u0915\u0947 \u0926\u094D\u0935\u093E\u0930\
  \u093E) \u0938\u0947 \u0930\u093F\u092A\u094D\u0932\u0947\u0938 \u0915\u0930\u0928\
  \u093E\u0964."
title: "\u092A\u0948\u091F\u0930\u094D\u0928 \u0938\u0947 \u092E\u0947\u0932 \u0916\
  \u093E\u0924\u0947 \u0905\u0915\u094D\u0937\u0930\u094B\u0902 \u0915\u094B \u0939\
  \u091F\u093E\u0928\u093E"
weight: 5
---

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
