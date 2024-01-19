---
title:                "पाठ की खोज और प्रतिस्थापन"
html_title:           "Bash: पाठ की खोज और प्रतिस्थापन"
simple_title:         "पाठ की खोज और प्रतिस्थापन"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

टेक्स्ट की खोज और बदलाव से मेरा तात्पर्य है, संगठनात्मक और संयात डेटा में किसी विशेष शब्द या वाक्यांश को पहचानना और उसे बदलना। प्रोग्रामर्स इसे हाई लेवल फ़ंक्शन और डाटा संस्थापन को सुनिश्चित करने के लिए करते हैं।

## कैसे करें:

लेखक को Fish Shell में टेक्स्ट खोजने और बदलने के लिए निम्नलिखित सीधे कोड उदाहरण देखें:

```Fish Shell
set v 'Hello, world!'
echo $v | string replace -r 'world' 'Fish Shell'
```

उसे निष्पादित करने पर, आपको निम्नलिखित आउटपुट मिलेगा:

```
Hello, Fish Shell!
```

## गहन अध्ययन

टेक्स्ट की खोज और बदलाव की क्रियाएँ कंप्यूटर विज्ञान के प्रारम्भ से ही हमारे साथ रही हैं। Fish Shell इस कौशल को अद्वितीयता, तार्किक संरचना, और सुविधाजनक सिंटैक्स के साथ जोड़ता है। वैकल्पिक तकनीकों में sed और awk का उपयोग किया जा सकता है, लेकिन Fish Shell के string replace द्वारा खोज और बदलाव सबसे सीधा और आसान है।

## और भी देखें:

1. सामान्य खोज और बदलाव तकनीकों के बारे में अधिक जानकारी के लिए, देखें [https://www.gnu.org/software/sed/manual/sed.html](https://www.gnu.org/software/sed/manual/sed.html).
2. Fish Shell के और टेक्स्ट मैनिपुलेशन कार्यों के बारे में, देखें [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html).