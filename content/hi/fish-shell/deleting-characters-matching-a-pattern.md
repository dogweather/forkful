---
title:                "पैटर्न से मेल खाते अक्षरों को हटाना"
aliases:
- hi/fish-shell/deleting-characters-matching-a-pattern.md
date:                  2024-01-20T17:42:36.129656-07:00
model:                 gpt-4-1106-preview
simple_title:         "पैटर्न से मेल खाते अक्षरों को हटाना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/deleting-characters-matching-a-pattern.md"
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
