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

## क्या और क्यों?
अगर आप किसी डायरेक्टरी का अस्तित्व जांचना चाहते हैं, तो आप बस यह देखना चाहते हैं कि उस नाम से कोई फ़ोल्डर मौजूद है या नहीं। प्रोग्रामर्स इस चीज़ को करते हैं ताकि वे अपने कोड को और बेहतर बना सकें। 

## कैसे करें:
```Python
import os

# उस डायरेक्टरी का अस्तित्व जांचें जिसको आप चाहते हैं
if os.path.exists("directory_name"):
  # अगर अस्तित्व है तो संदेश प्रिंट करें
  print("यह डायरेक्टरी मौजूद है।")
else:
  # अगर अस्तित्व नहीं है तो संदेश प्रिंट करें
  print("यह डायरेक्टरी मौजूद नहीं है।")
```

आउटपुट:
```
यह डायरेक्टरी मौजूद है।
```

## गहराई में जाएँ:
इस चीज़ को करने के पीछे का इतिहास है जो कि डायरेक्टरी का अस्तित्व जांचने की आसान और तेज़ तरीके को बनाने का फ़ैसला निकाला गया था। आप अन्य तरीकों से भी कर सकते हैं, जैसे इस्तेमाल करके: `path.isdir("directory_name")` और `os.stat("directory_name").st_mode & 0o170000 == 0o040000`। इम्प्लीमेंटेशन डिटेल की बात करें, यह फ़ंक्शन फ़ाइल सिस्टम का समुचित अपवर्तक का उपयोग करता है।

## इसके अलावा देखें:
संबंधित स्रोतों के लिए नीचे दिए गए लिंक देखें:
- [Python डॉक्यूमेंटेशन हिंदी में](https://www.jeffknupp.com/blog/2013/02/27/open-sourcing-a-python-project-the-right-way/)
- [Python के अस्तित्व से जुड़ा वोटिंग आर्गुमेंट](https://www.tutorialspoint.com/python/python_check_if_file_exists.htm)
- [पायथन 3 सस्ती अध्यतम मामला](https://medium.com/swlh/checking-for-cheap-tricks-in-python-3-e18ad38b5fff)