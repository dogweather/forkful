---
title:                "सबस्ट्रिंग्स निकालना"
html_title:           "Clojure: सबस्ट्रिंग्स निकालना"
simple_title:         "सबस्ट्रिंग्स निकालना"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
सबस्ट्रिंग निकालना (Extracting Substring), किसी बड़ी स्ट्रिंग से छोटे स्ट्रिंग को प्राप्त करना है। प्रोग्रामर्स इसे डाटा परिष्करण, रिपोर्टिंग, और क्वैरी बनाने के लिए करते हैं।

## करने की विधि:
Bash में, हम '${string:position:length}' संदर्भ का उपयोग करके सबस्ट्रिंग निकाल सकते हैं। आइए इसका प्रयोग देखें।

```Bash
#!/bin/bash
string="Hello, World!"
echo ${string:7:5}
```
इसके आउटपुट में 'World' मिलेगी।

## गहराई में जाने के लिए:
Bash में सबस्ट्रिंग निकालने की क्रिया प्राचीन Unix तरीके जैसे 'cut' और 'awk' से अधिक समय बचाती है। वैकल्पिक रूप से, 'sed' और 'perl' के जैसे उपकरण भी मिलते हैं, लेकिन वे अधिक क्लियर और उच्च प्रदर्शन वाली बातें नहीं हैं। बाश में सबस्ट्रिंग को निकालने की क्रिया tokenize और regex में से कोई भी उपयोग नहीं करता है जो इसे अधिक तेज बनाती है।

## देखने के लिए भी:
आप और जानकारी और संसाधनों के लिए निम्नलिखित लिंक देख सकते हैं:
- GNU Bash Reference: [https://www.gnu.org/software/bash/manual/bash.html](https://www.gnu.org/software/bash/manual/bash.html)
- Advanced Bash Scripting Guide: [https://tldp.org/LDP/abs/html/string-manipulation.html](https://tldp.org/LDP/abs/html/string-manipulation.html)
- 'Extracting Substrings in Bash' स्टैक ओवरफ्लो पोस्ट: [https://stackoverflow.com/q/428109/https://stackoverflow.com/q/428109](https://stackoverflow.com/q/428109/https://stackoverflow.com/q/428109)