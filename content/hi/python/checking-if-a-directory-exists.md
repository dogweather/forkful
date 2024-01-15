---
title:                "डायरेक्टरी का अस्तित्व जांच करना"
html_title:           "Python: डायरेक्टरी का अस्तित्व जांच करना"
simple_title:         "डायरेक्टरी का अस्तित्व जांच करना"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

#क्यों

कई बार हमें अपनी पाठ्यक्रम या अन्य कंप्यूटरीकृत कार्य श्रृंखला में एक निश्चित फोल्डर होने की आवश्यकता होती है। इसलिए इसका जांच करना एक अहम कार्य है।

#कैसे करें

```python
import os

folder_path = "C:/Users/Username/Documents/"

if os.path.exists(folder_path):
    print("The directory exists!")
else:
    print("The directory does not exist!")
```

यहां हमने os मॉड्यूल का उपयोग करके एक फ़ोल्डर का पथ बनाया है और फिर os.path.exists() फ़ंक्शन को इसके साथ दिया है। यदि फ़ोल्डर मौजूद है, तो यह प्रिंट करेगा "The directory exists!" और अन्यथा यह "The directory does not exist!" प्रिंट करेगा।

#गहराई में जाएं

जब हम इस जांच को कोड करते हैं, तो हम प्रथमतः os.path आइनाम जाँच करते हैं कि क्या फ़ोल्डर मौजूद है या नहीं। यदि यह मौजूद है, तो हम फिर से जाँच करते हैं कि क्या यह एक वास्तविक फ़ोल्डर है या नहीं। यदि हमें एक फ़ोल्डर के नाम में स्पेस या किसी अन्य विशेष चरित्र पाते हैं, तो हम इसे निर्धारित कर सकते हैं कि यह वास्तव में एक फ़ोल्डर है। इसलिए, जांच करने से पहले फ़ोल्डर का पूरा पथ बना लें ताकि हम यदि कोई ऐसा विशेष चरित्र है तो उसे अनदेखा कर सकें।

#देखें भी

- [Python os module documentation](https://docs.python.org/3/library/os.html)
- [os.path module documentation](https://docs.python.org/3/library/os.path.html)