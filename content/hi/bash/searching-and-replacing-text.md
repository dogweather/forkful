---
title:                "पाठ की खोज और प्रतिस्थापन"
html_title:           "Bash: पाठ की खोज और प्रतिस्थापन"
simple_title:         "पाठ की खोज और प्रतिस्थापन"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

सर्चिंग और रिप्लेसिंग टेक्स्ट का काम एक या अधिक फाइलों में जब हम किसी टेक्स्ट या पैटर्न की खोज कर उसे दूसरे टेक्स्ट से बदलना चाहते हैं वह होता है। यह कार्य काफी आम है क्योंकि यह प्रोग्रामर्स के लिए टाइम-सेविंग और एफिशियेंट तरीका होता है टेक्स्ट फाइल्स को अपडेट करने का। 

## कैसे करें:

निम्नलिखित कोड स्निपेट में, आप टेक्स्ट खोजने और बदलने के लिए `sed` कमांड विलक्षणता को देख सकते हैं।

```Bash
# ऑरिजनल फ़ाइल
echo "Hello World" > example.txt

# टेक्स्ट खोजें और बदलें
sed 's/World/Universe/' example.txt
```

स्क्रिप्ट चलाने पर ‘Hello Universe’ आउटपुट में दिखाई देगा।

## गहराई विवेचन

1. हिस्टोरिकल कन्टेक्ट: `sed` (Stream EDitor) यह एक पावरफुल और फ्लेक्सिबल खोज और रिप्लेस उपकरण है। इसे 1974 में मार्क डोद्नेट द्वारा विकसित किया गया था। 
2. विकल्प: `awk`, `grep`, और `Perl` भी खोजने और बदलने के उपकरणों के रूप में उपयोग होते हैं।
3. इंप्लिमेंटेशन विवरण: `sed` कमांड का उपयोग करके, हम एक पैटर्न या रेगुलर एक्सप्रेशन की खोज कर सकते हैं और पूरी फ़ाइल में उसे दूसरे टेक्स्ट के साथ रिप्लेस कर सकते हैं।

## अन्य स्रोत देखें:

* [GNU Sed Documentation](https://www.gnu.org/software/sed/manual/sed.html)
* [GeeksforGeeks Sed Command Tutorial](https://www.geeksforgeeks.org/sed-command-in-linux-unix-with-examples/)