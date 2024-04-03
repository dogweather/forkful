---
date: 2024-01-20 17:57:57.021776-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) ."
lastmod: '2024-03-13T22:44:52.597322-06:00'
model: gpt-4-1106-preview
summary: .
title: "\u092A\u093E\u0920 \u0916\u094B\u091C\u0928\u093E \u0914\u0930 \u092C\u0926\
  \u0932\u0928\u093E"
weight: 10
---

## How to: (कैसे करें:)
```Bash
# एक फाइल में 'apple' को 'orange' से बदलने के लिए
sed 's/apple/orange/' fruits.txt

# सभी occurrences को रिप्लेस करना
sed -i 's/apple/orange/g' fruits.txt

# आउटपुट के साथ
echo "apple banana apple" | sed 's/apple/orange/g'
# आउटपुट: orange banana orange
```

## Deep Dive (गहराई में):
सर्च और रिप्लेस कमांड `sed` यानी 'stream editor' UNIX सिस्टम के पुराने दिनों से है। `sed` एक पावरफुल टूल है जो पाइपलाइन्स और स्क्रिप्ट्स में टेक्स्ट प्रोसेसिंग के लिए अक्सर इस्तेमाल होता है। `awk`, `grep`, `perl`, और `python` भी टेक्स्ट मणिपुलेशन के लिए विकल्प हो सकते हैं, पर `sed` की सिंप्लिसिटी और दक्षता इसे विशेष बनाती है। `sed` पैटर्न स्पेस का इस्तेमाल करके फाइल की प्रत्येक लाइन की प्रोसेसिंग करता है और कमांड लाइन पर दिए गए 's' फ्लैग (सब्स्टिट्यूट कमांड) के साथ पैटर्न मैच करने पर टेक्स्ट को बदल देता है।

## See Also (और भी देखें):
1. GNU sed manual: [https://www.gnu.org/software/sed/manual/sed.html](https://www.gnu.org/software/sed/manual/sed.html)
2. Regular Expressions (RegEx) Guide: [https://www.regular-expressions.info/](https://www.regular-expressions.info/)
3. Linux Shell Scripting Tutorial: [https://bash.cyberciti.biz/guide/Main_Page](https://bash.cyberciti.biz/guide/Main_Page)
4. Advanced Bash-Scripting Guide: [https://www.tldp.org/LDP/abs/html/](https://www.tldp.org/LDP/abs/html/)
