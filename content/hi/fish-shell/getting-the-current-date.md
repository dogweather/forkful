---
title:                "वर्तमान तारीख प्राप्त करना"
html_title:           "C#: वर्तमान तारीख प्राप्त करना"
simple_title:         "वर्तमान तारीख प्राप्त करना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## क्या और क्यों? 

वर्तमान दिनांक प्राप्त करना यह जानने की प्रक्रिया होती है कि आज कौन सा दिन है। प्रोग्रामर्स उसे विभिन्न कारणों से करते हैं, जैसे लगता है, डाटा धारावाहिकीकरण, लॉग्स और हिटोरिकल डाटा को ट्रैक करना।

## कैसे करें:

वर्तमान तारीख प्राप्त करने के लिए, फिश शेल में निम्नलिखित कोड का उपयोग करें:

```Fish Shell
date
```

आउटपुट ऐसा होगा:

```Fish Shell
Tue 27 April 2021 16:11:19 IST
```
## गहरी जांच:

1) ऐतिहासिक संदर्भ: 'date' कमांड Unix और Linux ऑपरेटिंग सिस्टम में लंबे समय से उपलब्ध है। यह POSIX की मान्यता प्राप्त कमांड है क्योंकि यह सर्वत्र उपलब्ध होती है। 

2) विकल्प: आप 'strftime' कमांड का उपयोग कर सकते हैं जो वर्तमान तारीख को विभिन्न प्रारूपों में प्रदर्शित करता है। उदाहरण के लिए, निम्नलिखित कोड:

```Fish Shell
date "+%d-%m-%Y"
```

3) क्रियान्वयन विवरण: 'date' कमांड ऑपरेटिंग सिस्टम के कर्नल से सिस्टम का समय प्राप्त करती है। यह POSIX क्षेत्र या सिस्टम पर निर्भर कर सकता है।

## देखें भी:

1) डेट कमांड का प्रलेखन ([Fish Shell - date](https://fishshell.com/docs/current/cmds/date.html))
2) strftime कमांड का प्रलेखन ([Fish Shell - strftime](https://fishshell.com/docs/current/cmds/strftime.html))
3) POSIX के बारे में अधिक जानकारी के लिए ([POSIX](https://en.wikipedia.org/wiki/POSIX))