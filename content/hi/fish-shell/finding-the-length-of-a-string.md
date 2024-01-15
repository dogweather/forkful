---
title:                "तंबु की लंबाई ढूंढना"
html_title:           "Fish Shell: तंबु की लंबाई ढूंढना"
simple_title:         "तंबु की लंबाई ढूंढना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/finding-the-length-of-a-string.md"
---

{{< edit_this_page >}}

## क्यों

कोई भी पाठ स्ट्रिंग की लंबाई को जानना चाहेगा ताकि वे किसी भी रचना में पाठ स्ट्रिंग को सही तरीके से उपयोग कर सकें।

## कैसे करें

फिश शैल (वर्तमान संस्करण) में पाठ स्ट्रिंग की लंबाई को जानने के लिए आप निम्नानुसार कर सकते हैं। सबसे पहले, एक पाठ स्ट्रिंग बनाएं जिसकी आप लंबाई जानना चाहते हैं। उसके बाद, निम्नलिखित कोड को `length.sh` नाम से सहेजें:

```
#! /usr/bin/env fish 

echo "पाठ स्ट्रिंग लंबाई: (string length (echo $argv))"
```

अंत में, टर्मिनल में निम्नलिखित कमांड को चलाएं:

```
$ ./length.sh Hello World
```

इस उदाहरण में, पाठ स्ट्रिंग `Hello World` की लंबाई `11` होगी।

## गहराई में खोज

पाठ स्ट्रिंग की लंबाई को प्राप्त करने के लिए, फिश शैल का बारामदा कार्य `string length` का उपयोग किया जाता है। यह कार्य दिए गए पाठ स्ट्रिंग की लंबाई को निर्धारित करता है। फिश शैल में `string length` को एक प्रभावी तरीके से उपयोग करने के लिए, आपको पाठ स्ट्रिंग को मूल्यांकन करना हो सकता है जिससे आप उस का उपयोग अन्य बारामदा कार्यों में कर सकें।

## देखें भी

- [फिश शैल विकी](https://fishshell.com/docs/current/index.html)
- [फिश शैलल सीखें](https://fishshell.com/docs/current/tutorial.html)