---
title:    "Bash: कम्प्यूटर प्रोग्रामिंग पर लेखन: कमांड रैखिक तर्कों को पढ़ना"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## क्यों

कमांड लाइन आर्ग्यूमेंट पढ़ना तब लोगों के लिए महत्वपूर्ण हो सकता है जब वे बाश प्रोग्रामिंग के बारे में सीखना शुरू करते हैं। इससे वे संबंधित विषयों में अधिक जान सकते हैं और बाश में निपुण हो सकते हैं।

## कैसे करें

बाश में कमांड लाइन आर्ग्यूमेंट पढ़ने के लिए, हम स्पेशल निर्देश ग्राहक "$ $" का उपयोग करते हैं। इसमें "$ 0" को उदाहरण रूप में लेने से हम अपने स्क्रिप्ट का नाम प्राप्त कर सकते हैं। यहां एक उदाहरण है:

```Bash
#!/bin/bash
echo "यह स्क्रिप्ट का नाम है: $0"
```
इसका आउटपुट निम्न रूप में होगा:
```
यह स्क्रिप्ट का नाम है: script.sh
```
आप में से अधिकांश लोगों को स्क्रिप्ट में उपलब्ध कमांड लाइन आर्ग्यूमेंट के बारे में अवगत नहीं होते हैं। इसलिए, इस उदाहरण में हमने इसे सादा बनाया है ताकि आप आसानी से समझ सकें। लेकिन बाश में आर्ग्यूमेंट के अन्य उपयोग भी हैं, जो कि आप अधिक जानना चाहेंगे।

## गहराई में

बाश में कमांड लाइन आर्ग्यूमेंट पढ़ना अधिक गहराई के साथ कुछ तरीकों को सीखने का एक अच्छा तरीका है। आप स्पेशल पैरामीटर्स, जैसे "$@" और "$#" को भी सीख सकते हैं। इन सभी का उपयोग करते हुए आप अपने स्क्रिप्ट को और मजबूत बना सकते हैं और उसमें अध