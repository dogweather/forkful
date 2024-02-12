---
title:                "स्ट्रिंग इंटरपोलेशन"
aliases: - /hi/fish-shell/interpolating-a-string.md
date:                  2024-01-20T17:51:02.354349-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग इंटरपोलेशन"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/interpolating-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
इंटरपोलेटिंग स्ट्रिंग का मतलब होता है वेरिएबल्स या एक्सप्रेशन्स को स्ट्रिंग्स के अंदर डायरेक्ट इंसर्ट करना। प्रोग्रामर्स इसे इसलिए करते हैं ताकि वे डायनेमिक वैल्यूज़ को आसानी से मैनेज और डिस्प्ले कर सकें।

## How to: (कैसे करें:)
```Fish Shell
# वेरिएबल सेट करें
set name "दुनिया"

# स्ट्रिंग इंटरपोलेशन का प्रयोग
echo "नमस्ते, $name!"

# आउटपुट
नमस्ते, दुनिया!
```

## Deep Dive (गहराई में जानकारी)
Fish Shell में स्ट्रिंग इंटरपोलेशन शुरुआत से ही अन्य शेल्स की तुलना में साफ-सुथरा रहा है। जैसे, Bash शेल में डबल कोट्स के अंदर `$` साइन का इस्तेमाल करके इंटरपोलेशन किया जाता है, Fish में भी यही तरीका है लेकिन यहाँ ब्रेसेस `{}` की जरूरत नहीं होती।

Fish Shell में इंटरपोलेटेड स्ट्रिंग्स को और भी प्रभावी तरीके से इस्तेमाल किया जा सकता है, जैसे कमांड सब्स्टीट्यूशन के लिए `(command)` संरचना का प्रयोग, जो सीधे स्ट्रिंग के अंदर ही रिजल्ट डाल देता है। यह फीचर कोड को काफी रीडेबल और मेंटेन करने योग्य बनाता है।

## See Also (और जानकारी के लिंक)
- [Fish Documentation on String Interpolation](https://fishshell.com/docs/current/index.html#syntax-command-sub)
- [Learn X in Y minutes for Fish](https://learnxinyminutes.com/docs/fish/)
- [Fish Shell GitHub Repository](https://github.com/fish-shell/fish-shell)
