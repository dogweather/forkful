---
date: 2024-01-20 17:37:53.333449-07:00
description: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u0932\
  \u094B\u0905\u0930 \u0915\u0947\u0938 \u092E\u0947\u0902 \u092C\u0926\u0932\u0928\
  \u093E \u092E\u0924\u0932\u092C \u0909\u0938\u092E\u0947\u0902 \u092E\u094C\u091C\
  \u0942\u0926 \u0938\u093E\u0930\u0947 \u0905\u0915\u094D\u0937\u0930\u094B\u0902\
  \ \u0915\u094B \u091B\u094B\u091F\u0947 \u0905\u0915\u094D\u0937\u0930\u094B\u0902\
  \ \u092E\u0947\u0902 \u092A\u0930\u093F\u0935\u0930\u094D\u0924\u093F\u0924 \u0915\
  \u0930\u0928\u093E\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\
  \ \u0907\u0938\u0947 \u0924\u092C \u0915\u0930\u0924\u0947 \u0939\u0948\u0902 \u091C\
  \u092C case sensitivity \u0915\u0947 \u0915\u093E\u0930\u0923\u094B\u0902\u2026"
lastmod: '2024-03-13T22:44:52.600511-06:00'
model: gpt-4-1106-preview
summary: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u0932\u094B\
  \u0905\u0930 \u0915\u0947\u0938 \u092E\u0947\u0902 \u092C\u0926\u0932\u0928\u093E\
  \ \u092E\u0924\u0932\u092C \u0909\u0938\u092E\u0947\u0902 \u092E\u094C\u091C\u0942\
  \u0926 \u0938\u093E\u0930\u0947 \u0905\u0915\u094D\u0937\u0930\u094B\u0902 \u0915\
  \u094B \u091B\u094B\u091F\u0947 \u0905\u0915\u094D\u0937\u0930\u094B\u0902 \u092E\
  \u0947\u0902 \u092A\u0930\u093F\u0935\u0930\u094D\u0924\u093F\u0924 \u0915\u0930\
  \u0928\u093E\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\
  \ \u0907\u0938\u0947 \u0924\u092C \u0915\u0930\u0924\u0947 \u0939\u0948\u0902 \u091C\
  \u092C case sensitivity \u0915\u0947 \u0915\u093E\u0930\u0923\u094B\u0902\u2026"
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u091B\u094B\
  \u091F\u0947 \u0905\u0915\u094D\u0937\u0930\u094B\u0902 \u092E\u0947\u0902 \u092A\
  \u0930\u093F\u0935\u0930\u094D\u0924\u093F\u0924 \u0915\u0930\u0928\u093E"
weight: 4
---

## What & Why? (क्या और क्यों?)
स्ट्रिंग को लोअर केस में बदलना मतलब उसमें मौजूद सारे अक्षरों को छोटे अक्षरों में परिवर्तित करना। प्रोग्रामर इसे तब करते हैं जब case sensitivity के कारणों से तुलना या डाटा संग्रह करना होता है।

## How to: (कैसे करें)
```Bash
# वेरिएबल में स्ट्रिंग को लोअर केस में सेट करना
my_string="Hello World!"
lowercase_string=$(echo "$my_string" | tr '[:upper:]' '[:lower:]')
echo $lowercase_string  # hello world!

# Bash 4.0+ का उपयोग करना
declare -l lowercase_string2="Hello World Again!"
echo $lowercase_string2  # hello world again!
```

## Deep Dive (गहराई से समझिए)
जब लिनक्स और यूनिक्स सिस्टम में शेल स्क्रिप्टिंग आरम्भ हुई थी, तब `tr` या "translate characters" उपकरण का इस्तेमाल होता था। यह यूनिक्स की प्रारंभिक features में से एक है। बाश वर्जन 4.0+ से, डेवलपर्स को case manipulation के लिए built-in support मिली, जिससे पहले के तरीके (`tr`, `awk`, etc.) को अब उतना इस्तेमाल नहीं किया जाता। `declare` statement के `-l` और `-u` options से क्रमश: lowercase और uppercase में बदलाव कर सकते हैं।

## See Also (और जानकारी के लिए)
- Bash Manual: [https://www.gnu.org/software/bash/manual/](https://www.gnu.org/software/bash/manual/)
- Advanced Bash-Scripting Guide: [https://tldp.org/LDP/abs/html/](https://tldp.org/LDP/abs/html/)
- `tr` Command Tutorial: [https://www.geeksforgeeks.org/tr-command-in-unix-linux-with-examples/](https://www.geeksforgeeks.org/tr-command-in-unix-linux-with-examples/)
- Stack Overflow discussion on string manipulation: [https://stackoverflow.com/questions/tagged/string+manipulation+bash](https://stackoverflow.com/questions/tagged/string+manipulation+bash)
