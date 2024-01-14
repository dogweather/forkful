---
title:                "Fish Shell: स्टैंडर्ड त्रुटि पर लिखना"
programming_language: "Fish Shell"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## क्यों

वर्तमान युग में प्रोग्रामिंग का हमेशा से ही बढ़ता हुआ चाहते में आप लोगों के लिए हमारा आज का ब्लॉग पोस्टबता रहा है- क्यों लिखें standard error में प्रोग्रामिंग करना चाहिए।

## कैसे करें

Fish Shell में standard error में लिखने के लिए हम निम्नलिखित command का प्रयोग कर सकते हैं:

```
echo "स्वागत है"
```

और outputको *stderr* में लिखने के लिए निम्नलिखित command का प्रयोग कर सकते हैं:

```
echo "यह एरर है" >&2
```

आप हमारे द्वारा दी गई coding examples को अपने सिस्टम पर आजमा सकते हैं। यहां हमारे सिस्टम का output है:

स्वागत है

यह एरर है

## गहराई में जाएं

स्टैंडर्ड एरर में लिखना अपने कोड को लंबे समय तक चलाने के दौरान उपयोगी हो सकता है। इससे आपके कोड में स्थानों पर गलतियां होने पर भी आपको पता चल सकता है और उन्हें ठीक किया जा सकता है। इसके अलावा, एरर कोड के साथ काम करते हुए आपको समय बचाने में भी मदद मिल सकती है।

## और देखें

[Fish Shell के साथ शुरू करें](https://fishshell.com/)

[Fish Shell के बारे में और अधिक जानें](https://fishshell.com/docs/current/tutorial.html#exercise-your-echoes)

[Fish Shell बनाने की विधि (अधिक प्रोग्रामिंग के साथ आप्स बनाने की)](https://fishshell.com/docs/current/index.html#creating-new-fish-plugins)

_शुक्रिया। आपके साथ पूरा ट्यूटोरियल समाप्त हो गया है। शेयर करें और सही मार्गदर्शन दें। साथ ही हमारे ब्लॉग को सब्सक्राइब भी ज़रूर करें।_

## इन्हें भी देखें

* [Shell Scripting सी