---
title:                "डायरेक्टरी मौजूद है या नहीं यह जांचना"
html_title:           "Go: डायरेक्टरी मौजूद है या नहीं यह जांचना"
simple_title:         "डायरेक्टरी मौजूद है या नहीं यह जांचना"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## क्या और क्यों? 

डायरेक्टरी मौजूद है या नहीं, इसे जांचना से तात्पर्य है कि हम कुछ कोड चलाते समय एक निर्दिष्ट फ़ाइल संगठन योजना (डायरेक्टरी) की उपस्थिति की सत्यापन करते हैं। प्रोग्रामर्स इसे ताकि यह सुनिश्चित कर सकें कि उस डायरेक्टरी में वे कार्य कर सकें जो वे चाहते हैं, और अगर यह मौजूद नहीं है, तो उन्हें उचित त्रुटि चेतावनी प्रदान करें। 

## कैसे: 

Python के `os` लाइब्रेरी का उपयोग करके हम यह जांच सकते हैं:

```Python
import os

def check_directory(directory):
    if os.path.isdir(directory):
        print("\"{}\" डायरेक्टरी मौजूद है।".format(directory))
    else:
        print("\"{}\" डायरेक्टरी मौजूद नहीं है।".format(directory))

check_directory('my_directory')
```

## गहराई में:

यह विधि अच्छी है लेकिन इसका उपयोग करने से पहले, Python `os` मॉड्यूल की `path` सब-मॉड्यूल का उपयोग करने या अन्य विकल्पों, जैसे कि `pathlib` मॉड्यूल का उपयोग करने पर विचार करें। `os.path.isdir()` वास्तव में एक फ़ाइलिंग सिस्टम कॉल उठाता है, और अगर आप बहुत अक्सर यह जांचने का प्रयास करते हैं, तो यह आपके कोड को धीमा कर सकता है। 

## इसके अलावा देखें: 

1. Python डाक्युमेंटेशन - os.path:  https://docs.python.org/3/library/os.path.html
2. Python डाक्युमेंटेशन - pathlib: https://docs.python.org/3/library/pathlib.html