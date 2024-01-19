---
title:                "डायरेक्टरी मौजूद है या नहीं, यह जांचना"
html_title:           "Bash: डायरेक्टरी मौजूद है या नहीं, यह जांचना"
simple_title:         "डायरेक्टरी मौजूद है या नहीं, यह जांचना"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# बैश प्रोग्रामिंग: निर्देशिका मौजूद है या नहीं, कैसे चेक करें

## क्या और क्यों?
'निर्देशिका मौजूद है या नहीं' की जांच पाठ्यक्रम के तत्व को खोजती है एक सिस्टम पर एक विशेष निर्देशिका मौजूद है या नहीं। कार्यकर्ताओं को यह सुनिश्चित करना होता है ताकि उन्हें त्रुटियों से बचाया जा सके जो निर्देशिका ग़ैर-मौजूद होने पर उत्पन्न हो सकती हैं।

## कैसे करें:
```Bash
if [ -d "$DIRECTORY" ]; then
    echo "$DIRECTORY exists."
else
    echo "$DIRECTORY does not exist."
fi
```
उदाहरण का परिणाम:
```Bash
/mydirectory exists.
```
## गहरा डाइव:
बैश में निर्देशिका मौजूद है की जांच `-d` फ्लैग के साथ `if` संरचना का उपयोग करके की जाती है। इसे 1970 के दशक के बाद से उपयोग किया जा रहा है। `-e` फ्लैग एक वैकल्पिक विधि है जो एक फ़ाइल या निर्देशिका मौजूद है, यह जांच करता है।

## अन्य देखें:
1. [Bash Manual - Conditional Expressions](https://tldp.org/LDP/Bash-Beginners-Guide/html/sect_07_01.html)
2. [Bash if..else Statement](https://www.linuxize.com/post/bash-if-else-statement/)