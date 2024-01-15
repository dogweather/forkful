---
title:                "डायरेक्टरी का अस्तित्व जांचें"
html_title:           "Arduino: डायरेक्टरी का अस्तित्व जांचें"
simple_title:         "डायरेक्टरी का अस्तित्व जांचें"
programming_language: "Arduino"
category:             "Arduino"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/arduino/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# क्यों

क्या आपको कभी अपने Arduino प्रोजेक्ट में कुछ चीजों को स्टोर करने या लोड करने के लिए एक फ़ोल्डर जांचने की ज़रूरत हुई है? यदि हाँ, तो आपने शायद नोट पाया होगा कि फोल्डर क्या मौजूद है या नहीं। इसलिए हम आज Arduino में फ़ोल्डर के मौजूद होने की जांच कैसे करते हैं, इसके बारे में बात करेंगे।

## कैसे करें

क्या आप जानते हैं कि Arduino में फ़ोल्डर के मौजूद होने की जांच करने के लिए एक फ़ंक्शन होता है? हां, आपने सही सुना है! वह है `exists()` फ़ंक्शन। यह फ़ंक्शन दो तरीके से कॉल किया जा सकता है - `exists(path)` और `exists(dir, fileName)`। पहला तरीका हमें फ़ोल्डर की पथ को चेक करने के लिए देता है जबकि दूसरा तरीका हमें फ़ोल्डर में कौन सी फ़ाइल है उसका भी पता लगाने का अनुमान देता है। आइए एक उदाहरण के साथ देखें:

```arduino
bool exists(const char* path);

bool folderExists = exists("/Folder1/Folder2"); // Checks if "Folder2" exists inside "Folder1"
bool fileExists = exists("/Folder1/", "file.txt"); // Checks if "file.txt" exists inside "Folder1"
```

चिंता न करें, कॉड बोल्डर के उदाहरण में आपको केवल यह दिखाना है कि कैसे फ़ोल्डर `Folder1` और उसके अन्दर `Folder2` और `file.txt` की जांच करते हैं। इसके अलावा, फ़ोल्डर के केवल मौजूद होने की जांच करने के लिए, आप `exists()` फ़ंक्शन को केवल `Folder1` को ही पास कर सकते हैं। अब आपको अपने प्रोजेक्ट में फ़ोल्डर का मौजूद होना जाँचने में आसानी