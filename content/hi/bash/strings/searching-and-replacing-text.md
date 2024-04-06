---
date: 2024-01-20 17:57:57.021776-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u0938\u0930\
  \u094D\u091A \u0914\u0930 \u0930\u093F\u092A\u094D\u0932\u0947\u0938 \u0915\u092E\
  \u093E\u0902\u0921 `sed` \u092F\u093E\u0928\u0940 'stream editor' UNIX \u0938\u093F\
  \u0938\u094D\u091F\u092E \u0915\u0947 \u092A\u0941\u0930\u093E\u0928\u0947 \u0926\
  \u093F\u0928\u094B\u0902 \u0938\u0947 \u0939\u0948\u0964 `sed` \u090F\u0915 \u092A\
  \u093E\u0935\u0930\u092B\u0941\u0932 \u091F\u0942\u0932 \u0939\u0948 \u091C\u094B\
  \ \u092A\u093E\u0907\u092A\u0932\u093E\u0907\u0928\u094D\u0938 \u0914\u0930\u2026"
lastmod: '2024-04-05T22:51:07.276598-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) \u0938\u0930\u094D\u091A\
  \ \u0914\u0930 \u0930\u093F\u092A\u094D\u0932\u0947\u0938 \u0915\u092E\u093E\u0902\
  \u0921 `sed` \u092F\u093E\u0928\u0940 'stream editor' UNIX \u0938\u093F\u0938\u094D\
  \u091F\u092E \u0915\u0947 \u092A\u0941\u0930\u093E\u0928\u0947 \u0926\u093F\u0928\
  \u094B\u0902 \u0938\u0947 \u0939\u0948\u0964 `sed` \u090F\u0915 \u092A\u093E\u0935\
  \u0930\u092B\u0941\u0932 \u091F\u0942\u0932 \u0939\u0948 \u091C\u094B \u092A\u093E\
  \u0907\u092A\u0932\u093E\u0907\u0928\u094D\u0938 \u0914\u0930 \u0938\u094D\u0915\
  \u094D\u0930\u093F\u092A\u094D\u091F\u094D\u0938 \u092E\u0947\u0902 \u091F\u0947\
  \u0915\u094D\u0938\u094D\u091F \u092A\u094D\u0930\u094B\u0938\u0947\u0938\u093F\u0902\
  \u0917 \u0915\u0947 \u0932\u093F\u090F \u0905\u0915\u094D\u0938\u0930 \u0907\u0938\
  \u094D\u0924\u0947\u092E\u093E\u0932 \u0939\u094B\u0924\u093E \u0939\u0948\u0964\
  \ `awk`, `grep`, `perl`, \u0914\u0930 `python` \u092D\u0940 \u091F\u0947\u0915\u094D\
  \u0938\u094D\u091F \u092E\u0923\u093F\u092A\u0941\u0932\u0947\u0936\u0928 \u0915\
  \u0947 \u0932\u093F\u090F \u0935\u093F\u0915\u0932\u094D\u092A \u0939\u094B \u0938\
  \u0915\u0924\u0947 \u0939\u0948\u0902, \u092A\u0930 `sed` \u0915\u0940 \u0938\u093F\
  \u0902\u092A\u094D\u0932\u093F\u0938\u093F\u091F\u0940 \u0914\u0930 \u0926\u0915\
  \u094D\u0937\u0924\u093E \u0907\u0938\u0947 \u0935\u093F\u0936\u0947\u0937 \u092C\
  \u0928\u093E\u0924\u0940 \u0939\u0948\u0964 `sed` \u092A\u0948\u091F\u0930\u094D\
  \u0928 \u0938\u094D\u092A\u0947\u0938 \u0915\u093E \u0907\u0938\u094D\u0924\u0947\
  \u092E\u093E\u0932 \u0915\u0930\u0915\u0947 \u092B\u093E\u0907\u0932 \u0915\u0940\
  \ \u092A\u094D\u0930\u0924\u094D\u092F\u0947\u0915 \u0932\u093E\u0907\u0928 \u0915\
  \u0940 \u092A\u094D\u0930\u094B\u0938\u0947\u0938\u093F\u0902\u0917 \u0915\u0930\
  \u0924\u093E \u0939\u0948 \u0914\u0930 \u0915\u092E\u093E\u0902\u0921 \u0932\u093E\
  \u0907\u0928 \u092A\u0930 \u0926\u093F\u090F \u0917\u090F 's' \u092B\u094D\u0932\
  \u0948\u0917 (\u0938\u092C\u094D\u0938\u094D\u091F\u093F\u091F\u094D\u092F\u0942\
  \u091F \u0915\u092E\u093E\u0902\u0921) \u0915\u0947 \u0938\u093E\u0925 \u092A\u0948\
  \u091F\u0930\u094D\u0928 \u092E\u0948\u091A \u0915\u0930\u0928\u0947 \u092A\u0930\
  \ \u091F\u0947\u0915\u094D\u0938\u094D\u091F \u0915\u094B \u092C\u0926\u0932 \u0926\
  \u0947\u0924\u093E \u0939\u0948\u0964."
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
