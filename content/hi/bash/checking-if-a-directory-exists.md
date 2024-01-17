---
title:                "डायरेक्टरी का अस्तित्व जांचना"
html_title:           "Bash: डायरेक्टरी का अस्तित्व जांचना"
simple_title:         "डायरेक्टरी का अस्तित्व जांचना"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
डायरेक्टरी की जांच करना क्या है, और क्यों प्रोग्रामर इसे करते हैं। जब हम एक Bash स्क्रिप्ट लिखते हैं तो कई बार हमें कुछ करने से पहले या कुछ का पता चलने से पहले डायरेक्टरी की जांच करने की आवश्यकता होती है। 

## कैसे करें:
यदि हमारे पास एक डायरेक्टरी है जिसका नाम "Documents" है, और हमें देखना है कि इसे हमारी वर्तमान कार्य निर्देशिका में कैसे स्थानांतरित किया गया है, तो हम निम्न आलेख को लिख सकते हैं: 

```Bash 
if [ -d Documents ]; then 
    echo "Documents directory exists in the current working directory." 
fi 
``` 
**आउटपुट:** Documents directory exists in the current working directory. 

अगर "Documents" डायरेक्टरी मौजूद न होती, तो इसे नहीं लिखा जाता है। 

## गहराई में जाईये: 
**इतिहासी पृष्ठभूमि:** Bash शब्द अंग्रेजी का है और इसका अर्थ है "बाकशाल" या "खाली स्थान"। इसे 1980 के दशक में Brian Fox ने बनाया था और यह एक Shell scripting लैंग्वेज है। Bash स्क्रिप्ट के मध्य बाकी क्षेत्रों से आसान और अंकलनीय इसलिये है क्योंकि यह अत्याधिक भाषाएं लैटिन स्क्रिप्टों के साथ काम करता है।

**वैकल्पिक:** इनका खास उपयोगों के अलावा, हम अन्य परीक्षण योजनाएं भी उपयोग कर सकते हैं, जैसे "test -f Documents" या "[[ -e Documents ]]". यह सुनिश्चित करने के लिए है कि दी गई डायरेक्टरी नाम वाली अभिलेख वास्तविक में मौजूद है। 

**निष्पादन विवरण:** इस व्यवस्था को प्रोग्रामिंग भाषा के दूसरे भागों की तरह, Bash में "if-then" का इस्तेमाल करके किया जाता है। हम "if" के साथ "bracket" या "double bracket" से परीक्षण शर्तों को दर्शा सकते हैं। 

## इससे जुड़वां: 
अधिक जानकारी के लिए Bash और if-then परीक्षण के बारे में ये साइट देखें: 
- [Bash script basics](https://linuxconfig.org/bash-scripting-tutorial-for-beginners) 
- [If-then statements in Bash](https://www.thegeekstuff.com/2010/06/bash-if-statement-examples/)