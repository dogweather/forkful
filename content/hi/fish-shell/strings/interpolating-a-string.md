---
date: 2024-01-20 17:51:02.354349-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) ."
lastmod: '2024-04-05T21:53:54.979723-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0907\u0902\u091F\u0930\u092A\
  \u094B\u0932\u0947\u0936\u0928"
weight: 8
---

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
