---
title:                "Bash: एक तारीख को स्ट्रिंग में रूपांतरण करना"
simple_title:         "एक तारीख को स्ट्रिंग में रूपांतरण करना"
programming_language: "Bash"
category:             "Bash"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## क्यों:

जब हम बैश प्रोग्रामिंग में तारीख को स्ट्रिंग में रूपांतरित करते हैं, तो हमें दिन-महीने-वर्ष या समय को प्रतिस्थापित करने की आवश्यकता होती है। यह बहुत सारे उपयोगकर्ताओं के लिए उपयोगी हो सकता है, जैसे उन्हें एक फ़ाइल या डाटाबेस में तारीख स्टॉम्प की आवश्यकता हो सकती है।

## कैसे करें:

```
Bash date +"%m/%d/%Y"
```

यह कोड तारीख को मॉन्थ/डे/ईयर फॉर्मेट में स्ट्रिंग में प्रतिस्थापित करेगा। आप भी अपनी आवश्यकतानुसार तारीख को कस्टमाइज़ कर सकते हैं, जैसेः

```
Bash date +"%A, %B %d, %Y"
```

इस प्रकार आप सोमवार, जुलाई 05, 2021 जैसी तारीख को स्ट्रिंग में प्रतिस्थापित कर सकते हैं।

## गहराई में जाएं:

अगर आप चाहते हैं, आप भाषा फ़ाइल से छोटे या बड़े तारीख स्टॉम्प स्ट्रिंग को अपनी ज़रूरतों के अनुसार कस्टमाइज़ करने के लिए गहराई में जा सकते हैं। आप भी "man date" कमांड का उपयोग कर सकते हैं जो आपको भाषा और फ़ॉर्मेटिंग विकल्पों की सूची दिखाएगा।

## देखें भी:

- [Bash कॉमांड लाइन का प्रयोग कैसे करें](https://www.linux.com/topic/desktop/wget-and-curl-command-line-tips-httprdpcurlcom/)
- [Bash स्क्रिप्ट सीखने के लिए बेहतरीन मंच](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)
- [भाषा फ़ाइल कैसे बनाएं](https://unix.stackexchange.com/questions/115095/creating-an-output-file-with-generated-content-while-running-in-a-shell-loop)