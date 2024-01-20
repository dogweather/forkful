---
title:                "डायरेक्टरी का अस्तित्व जाँचना"
date:                  2024-01-20T14:58:18.229790-07:00
html_title:           "Elm: डायरेक्टरी का अस्तित्व जाँचना"
simple_title:         "डायरेक्टरी का अस्तित्व जाँचना"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
डायरेक्टरी मौजूद है या नहीं, यह जांचना यह सुनिश्चित करना है कि एक फ़ोल्डर सिस्टम में है भी या नहीं। प्रोग्रामर्स इसे इसलिए करते हैं ताकि वे फ़ाइल-रिलेटेड ऑपरेशन्स चलाने से पहले सुनिश्चित हो जाएं कि उन्हें एरर नहीं आएगी।

## कैसे करें? (How to:)
```Python
import os

# डायरेक्टरी का पथ
directory = "/path/to/directory"

# चेक करें कि डायरेक्टरी मौजूद है या नहीं
if os.path.isdir(directory):
    print("डायरेक्टरी मौजूद है। (Directory exists.)")
else:
    print("डायरेक्टरी मौजूद नहीं है। (Directory does not exist.)")
```

सैंपल आउटपुट:
```
डायरेक्टरी मौजूद है। (Directory exists.)
```
या
```
डायरेक्टरी मौजूद नहीं है। (Directory does not exist.)
```

## गहराई में (Deep Dive)
पहले, प्रोग्रामर्स `os.path.exists()` का इस्तेमाल करते थे, लेकिन यह फ़ंक्शन फ़ाइल्स के लिए भी `True` रिटर्न कर सकता था, न कि सिर्फ़ डायरेक्टरीज़ के लिए। इसलिए, `os.path.isdir()` का इस्तेमाल ज्यादा स्पष्ट है, जो केवल डायरेक्टरीज़ के लिए ही `True` रिटर्न करता है। 

लिनक्स जैसे प्लेटफार्मस पर जहाँ सिबिलिंक्स का इस्तेमाल होता है, `os.path.islink()` भी महत्वपूर्ण होता है। आप `os` मॉड्यूल के `makedirs()` फ़ंक्शन का इस्तेमाल करके डायरेक्टरी बना भी सकते हैं अगर वह मौजूद नहीं है।

एक आधुनिक विकल्प `pathlib` मॉड्यूल है, जो ऑब्जेक्ट-ओरिएंटेड एप्रोच को अपनाता है। `Path` क्लास का `exists()` मेथड काफी हद तक `os.path.exists()` जैसा ही है, लेकिन यह और भी कई सुविधाजनक मेथड्स प्रदान करता है।

## इसे भी देखें (See Also)
- [os.path.exists() और os.path.isdir() के बारे में ऑफिशियल पायथन डाक्यूमेंटेशन](https://docs.python.org/3/library/os.path.html)
- [pathlib मॉड्यूल के बारे में ऑफिशियल पायथन डाक्यूमेंटेशन](https://docs.python.org/3/library/pathlib.html)
- [सिबिलिंक्स के बारे में जानकारी](https://en.wikipedia.org/wiki/Symbolic_link)