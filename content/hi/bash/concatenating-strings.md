---
title:                "स्ट्रिंग्स को जोड़ना"
html_title:           "Bash: स्ट्रिंग्स को जोड़ना"
simple_title:         "स्ट्रिंग्स को जोड़ना"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
स्ट्रिंग कोंकैटिनेशन से मतलब होता है दो या दो से अधिक स्ट्रिंग्स को जोड़ना। प्रोग्रामर्स इस काम को ज्यादातर तब करते हैं जब उन्हें विभिन्न स्रोतों से प्राप्त डेटा को एक ही स्ट्रिंग में जोड़ना होता है। 

## कैसे करें:
यहां कुछ कोडिंग उदाहरण और आउटपुट दिए गए हैं:

```Bash
str1="नमस्ते, "
str2="दुनिया!"
echo $str1$str2
```

आउटपुट:

```Bash
नमस्ते, दुनिया!
```

## गहराई से जानें:
Bash में स्ट्रिंग्स को कोंकैटिनेट करने के लिए विभिन्न विधियाँ हैं, लेकिन उपरोक्त विधि सबसे साधारण और सीधी है। अन्य विधियों में "${str1}${str2}" जैसे variable brace expansions शामिल हैं। Bash में स्ट्रिंग कोंकैटिनेशन की क्षमता इसे बाकी शेल स्क्रिप्टिंग भाषाओं से अलग करती है।

## और भी देखें:
बैश स्ट्रिंग कोंकैटिनेशन के विषय में और अधिक जानकारी के लिए, आप देख सकते हैं:
1. [Bash Manual](https://www.gnu.org/savannah-checkouts/gnu/bash/manual/bash.html)  
2. [Advanced Bash-Scripting Guide](http://tldp.org/LDP/abs/html/string-manipulation.html)