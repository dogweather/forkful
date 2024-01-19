---
title:                "एक अस्थायी फ़ाइल बनाना"
html_title:           "Arduino: एक अस्थायी फ़ाइल बनाना"
simple_title:         "एक अस्थायी फ़ाइल बनाना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
अस्थाई फ़ाइलें ऐसी फ़ाइलें होती हैं जिन्हें आवश्यकता के समय तैयार किया जाता है और उनका उपयोग डाटा को अल्पकालिक रूप से संग्रहीत करने के लिए किया जाता है। कार्यक्रमकर्ताएं इसे तब तैयार करते हैं जब उन्हें कुछ डाटा को बाद में उपयोग करने के लिए संग्रहीत करना होता है। 

## कैसे करें:
Fish Shell में अस्थाई फ़ाइल बनाने के लिए, हम अस्थाई फ़ाइल पथ (tmpfile) नामक आदेश का उपयोग कर सकते हैं। निम्नलिखित कोड का उपयोग करते हुए यह काम किया जा सकता है:

```Fish_Shell
set tempfile (mktemp)
echo 'Temporary file created: ' $tempfile
```
ऊपर बताए गए कोड को चलाने पर आपको आउटपुट मिलेगा

```Fish_Shell
Temporary file created:  /tmp/tmp.59Lqqt7Yr7
```

## गहरी डुबकी 
Fish Shell की शुरुआत 2005 में की गई थी और इसमें अस्थाई फ़ाइलें बनाने की क्षमता उपलब्ध थी। अन्य विकल्पों के रूप में, आप Python, Java, और C++ जैसी अन्य भाषाओं में भी अस्थाई फ़ाइलें बना सकते हैं।

जब आप `mktemp` का उपयोग करके एक नई अस्थाई फ़ाइल बनाते हैं, तो Fish Shell ऑपरेटिंग सिस्टम से नई फ़ाइल बनाने के लिए मांग करता है जो आपके डाटा को सुरक्षित रखती है।  

## अन्य स्रोत देखें:
1. Fish shell डॉक्यूमेंटेशन: https://fishshell.com/docs/current/index.html
2. अस्थाई फ़ाइलें बनाने की जानकारी: https://en.wikipedia.org/wiki/Tmpfile
3. Python में अस्थाई फ़ाइलें बनाने का तरीका: https://docs.python.org/3/library/tempfile.html
4. C++ में अस्थाई फ़ाइलें बनाने का तरीका: https://www.cplusplus.com/reference/cstdio/tmpfile/