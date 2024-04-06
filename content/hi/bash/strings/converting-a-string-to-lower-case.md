---
date: 2024-01-20 17:37:53.333449-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902) \u091C\u092C\
  \ \u0932\u093F\u0928\u0915\u094D\u0938 \u0914\u0930 \u092F\u0942\u0928\u093F\u0915\
  \u094D\u0938 \u0938\u093F\u0938\u094D\u091F\u092E \u092E\u0947\u0902 \u0936\u0947\
  \u0932 \u0938\u094D\u0915\u094D\u0930\u093F\u092A\u094D\u091F\u093F\u0902\u0917\
  \ \u0906\u0930\u092E\u094D\u092D \u0939\u0941\u0908 \u0925\u0940, \u0924\u092C `tr`\
  \ \u092F\u093E \"translate characters\" \u0909\u092A\u0915\u0930\u0923 \u0915\u093E\
  \ \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0939\u094B\u0924\u093E \u0925\
  \u093E\u0964 \u092F\u0939 \u092F\u0942\u0928\u093F\u0915\u094D\u0938\u2026"
lastmod: '2024-04-05T22:51:07.279127-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902) \u091C\u092C \u0932\u093F\
  \u0928\u0915\u094D\u0938 \u0914\u0930 \u092F\u0942\u0928\u093F\u0915\u094D\u0938\
  \ \u0938\u093F\u0938\u094D\u091F\u092E \u092E\u0947\u0902 \u0936\u0947\u0932 \u0938\
  \u094D\u0915\u094D\u0930\u093F\u092A\u094D\u091F\u093F\u0902\u0917 \u0906\u0930\u092E\
  \u094D\u092D \u0939\u0941\u0908 \u0925\u0940, \u0924\u092C `tr` \u092F\u093E \"\
  translate characters\" \u0909\u092A\u0915\u0930\u0923 \u0915\u093E \u0907\u0938\u094D\
  \u0924\u0947\u092E\u093E\u0932 \u0939\u094B\u0924\u093E \u0925\u093E\u0964 \u092F\
  \u0939 \u092F\u0942\u0928\u093F\u0915\u094D\u0938 \u0915\u0940 \u092A\u094D\u0930\
  \u093E\u0930\u0902\u092D\u093F\u0915 features \u092E\u0947\u0902 \u0938\u0947 \u090F\
  \u0915 \u0939\u0948\u0964 \u092C\u093E\u0936 \u0935\u0930\u094D\u091C\u0928 4.0+\
  \ \u0938\u0947, \u0921\u0947\u0935\u0932\u092A\u0930\u094D\u0938 \u0915\u094B case\
  \ manipulation \u0915\u0947 \u0932\u093F\u090F built-in support \u092E\u093F\u0932\
  \u0940, \u091C\u093F\u0938\u0938\u0947 \u092A\u0939\u0932\u0947 \u0915\u0947 \u0924\
  \u0930\u0940\u0915\u0947 (`tr`, `awk`, etc.) \u0915\u094B \u0905\u092C \u0909\u0924\
  \u0928\u093E \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u0928\u0939\u0940\
  \u0902 \u0915\u093F\u092F\u093E \u091C\u093E\u0924\u093E\u0964 `declare` statement\
  \ \u0915\u0947 `-l` \u0914\u0930 `-u` options \u0938\u0947 \u0915\u094D\u0930\u092E\
  \u0936."
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u094B \u091B\u094B\
  \u091F\u0947 \u0905\u0915\u094D\u0937\u0930\u094B\u0902 \u092E\u0947\u0902 \u092A\
  \u0930\u093F\u0935\u0930\u094D\u0924\u093F\u0924 \u0915\u0930\u0928\u093E"
weight: 4
---

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
