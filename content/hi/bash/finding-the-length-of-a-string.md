---
title:                "स्ट्रिंग की लंबाई का पता लगाना"
html_title:           "Bash: स्ट्रिंग की लंबाई का पता लगाना"
simple_title:         "स्ट्रिंग की लंबाई का पता लगाना"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
श्रृंग लंबाई पाना एक प्रोग्रामिंग का प्रक्रिया है जिसमे एक स्ट्रिंग या शब्द की लंबाई को पता लगाया जाता है। प्रोग्रामर इसका उपयोग अपने कोड में लंबाई के संबंधित गणना और लूपिंग के लिए करते हैं।

## कैसे करें:
```Bash
string="Hello World"
echo ${#string}  # Output: 11
```
यहां हम स्ट्रिंग की लंबाई को `${#}` से प्राप्त कर सकते हैं। एक और तरीके से, `expr length $string` भी उपयोगी है।

## गहराई में जाएँ:
स्ट्रिंग की लंबाई को निकालने की प्रक्रिया प्राचीन समय से चली आ रही है। दरअसल, पहले कंप्यूटर प्रोग्राम में स्ट्रिंग को चुनाव और प्रोसेस करने के लिए लंबाई को दर्शाने के लिए परमाणु में यूनिकोड कोड का प्रयोग होता था। आजकल अलग-अलग भाषाओं में कोड किए गए हैं जो स्ट्रिंग की लंबाई निकालने के लिए अलग-अलग मेथड का उपयोग करते हैं।

स्ट्रिंग की लंबाई को निकालने के लिए कई अन्य विकल्प भी हैं, जैसे कि `wc -c` और `awk '{print length}'`। इन सभी अन्य विकल्पों में से आप अपने पसंद का चुन सकते हैं और इसके अलावा आपके द्वारा इस्तेमाल किए जाने वाले शेल स्क्रिप्ट में स्ट्रिंग की लंबाई को निकालने के लिए उपयोगी हो सकते हैं।

## अन्य स्रोतों का देखें:
- [Bash Shell Script to Check , Find And Get The Length Of a String Length of a Variable](https://www.cyberciti.biz/faq/unix-linux-bash-script-check-length-of-variable/)
- [Linuxize: How to Measure the Length of a String in Bash](https://linuxize.com/post/bash-string-length/)