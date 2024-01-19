---
title:                "कमांड लाइन तर्कों को पढ़ना"
html_title:           "Kotlin: कमांड लाइन तर्कों को पढ़ना"
simple_title:         "कमांड लाइन तर्कों को पढ़ना"
programming_language: "Bash"
category:             "Bash"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
सेवा लाइन तर्क पढ़ना मतलब है कुछ कार्यक्रम को इनपुट देना जो सेवा लाइन के माध्यम से गुजरती है। प्रोग्रामर्स क्यों ऐसा करते हैं? प्लेटफ़ॉर्म इंडिपेंडेंट और पुन: उपयोगी कोड लिखने के लिए - काम्याबी की कुंजी!

## कैसे करें:
जाओ और इस कोड को आजमाओ:

```Bash
#!/bin/bash
# bash कार्यक्रम तार्कों को प्रिंट करें
echo $0 # यह स्क्रिप्ट का नाम प्रिंट करेगा
echo $1 # यह पहला तर्क प्रिंट करेगा
echo $2 # यह दूसरा तर्क प्रिंट करेगा
```

जब आप `bash script.sh तर्क1 तर्क2` उपयोग करते हैं, यह आउटपुट देगा:

```Bash
script.sh
धार्क1
धार्क2
```

## गहरी डाइव
बैश स्क्रिप्टिंग का उपयोग UNIX और Linux सिस्टम प्रबंधन के लिए 1970 के दशक से किया जा रहा है। अल्टरनेटिव BASH ज़रूरत के अनुसार Perl, Python या Ruby हो सकता है। এই তর্ক স্঵ीকার করার পেছনের তার্কিক বিপরীত। * (सभी कार्यक्रम तर्क) चर का उपयोग करके या किंवादंतियों आदि के साथ संख्या बिक्री करके यहां और भी उच्च स्तरीय तर्क लगाया जा सकता है। 

## अन्य स्रोतों के लिए मिलिए:
1. [The Bash Guide](https://www.gnu.org/software/bash/manual/bash.html)
2. [Advanced Bash Scripting](https://tldp.org/LDP/abs/html/)
3. [Bash Scripting Tutorial](https://linuxconfig.org/bash-scripting-tutorial-for-beginners)