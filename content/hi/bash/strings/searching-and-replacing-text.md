---
aliases:
- /hi/bash/searching-and-replacing-text/
date: 2024-01-20 17:57:57.021776-07:00
description: "\u091F\u0947\u0915\u094D\u0938\u094D\u091F \u0938\u0930\u094D\u091A\u093F\
  \u0902\u0917 \u0914\u0930 \u0930\u093F\u092A\u094D\u0932\u0947\u0938\u093F\u0902\
  \u0917 \u0938\u0947 \u092E\u0924\u0932\u092C \u0939\u0948 \u0915\u093F\u0938\u0940\
  \ \u092B\u093E\u0907\u0932 \u092E\u0947\u0902 \u0936\u092C\u094D\u0926\u094B\u0902\
  \ \u0915\u094B \u0922\u0942\u0902\u0922\u0915\u0930 \u0909\u0928\u094D\u0939\u0947\
  \u0902 \u092C\u0926\u0932\u0928\u093E\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\
  \u0930\u093E\u092E\u0930\u094D\u0938 \u0907\u0938\u0947 \u0921\u093E\u091F\u093E\
  \ \u0915\u094B \u0905\u092A\u0921\u0947\u091F \u0915\u0930\u0928\u0947, \u0917\u0932\
  \u0924\u093F\u092F\u094B\u0902 \u0915\u094B \u0920\u0940\u0915 \u0915\u0930\u0928\
  \u0947 \u092F\u093E \u092B\u093E\u0907\u0932\u2026"
lastmod: 2024-02-18 23:09:03.625955
model: gpt-4-1106-preview
summary: "\u091F\u0947\u0915\u094D\u0938\u094D\u091F \u0938\u0930\u094D\u091A\u093F\
  \u0902\u0917 \u0914\u0930 \u0930\u093F\u092A\u094D\u0932\u0947\u0938\u093F\u0902\
  \u0917 \u0938\u0947 \u092E\u0924\u0932\u092C \u0939\u0948 \u0915\u093F\u0938\u0940\
  \ \u092B\u093E\u0907\u0932 \u092E\u0947\u0902 \u0936\u092C\u094D\u0926\u094B\u0902\
  \ \u0915\u094B \u0922\u0942\u0902\u0922\u0915\u0930 \u0909\u0928\u094D\u0939\u0947\
  \u0902 \u092C\u0926\u0932\u0928\u093E\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\
  \u0930\u093E\u092E\u0930\u094D\u0938 \u0907\u0938\u0947 \u0921\u093E\u091F\u093E\
  \ \u0915\u094B \u0905\u092A\u0921\u0947\u091F \u0915\u0930\u0928\u0947, \u0917\u0932\
  \u0924\u093F\u092F\u094B\u0902 \u0915\u094B \u0920\u0940\u0915 \u0915\u0930\u0928\
  \u0947 \u092F\u093E \u092B\u093E\u0907\u0932\u2026"
title: "\u092A\u093E\u0920 \u0916\u094B\u091C\u0928\u093E \u0914\u0930 \u092C\u0926\
  \u0932\u0928\u093E"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

टेक्स्ट सर्चिंग और रिप्लेसिंग से मतलब है किसी फाइल में शब्दों को ढूंढकर उन्हें बदलना। प्रोग्रामर्स इसे डाटा को अपडेट करने, गलतियों को ठीक करने या फाइल फार्मेटिंग में सुधार के लिए करते हैं।

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
