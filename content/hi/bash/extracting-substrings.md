---
title:                "सबस्ट्रिंग्स निकालना"
date:                  2024-01-20T17:46:18.398017-07:00
model:                 gpt-4-1106-preview
simple_title:         "सबस्ट्रिंग्स निकालना"

category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
उपश्रृंग (substring) निकालना मतलब किसी स्ट्रिंग के छोटे हिस्सों को पाना। प्रोग्रामर्स इसे तब करते हैं, जब उन्हें बड़ी स्ट्रिंग के कुछ विशेष खंडों की जरूरत होती है, जैसे फ़ाइलों के नाम, यूजर इनपुट, या कोड में पैटर्न मैचिंग के लिए। 

## How to: (कैसे करें:)
```Bash
# एक बेसिक उदाहरण
string="Welcome to Bash Programming"
# Offset और लेंथ के साथ सबस्ट्रिंग निकालना
substring=${string:11:4}
echo $substring  # Output: Bash

# शुरुआत के बिना और अंत तक सबस्ट्रिंग निकालना
substring=${string:11}
echo $substring  # Output: Bash Programming

# कहीं से भी शुरू करके एक शब्द निकालने के लिए डिलिमीटर का उपयोग
delimiter=" "
s=$string$delimiter

array=()
while [[ $s ]]; do
    array+=( "${s%%"$delimiter"*}" )
    s=${s#*"$delimiter"}
done

echo ${array[2]}  # Output: Bash
```

## Deep Dive (गहराई में डाइव करें):
Bash में सबस्ट्रिंग एक लोकप्रिय औजार है, लेकिन यह शुरूआत में नहीं था। पुराने शेल स्क्रिप्टिंग से उद्धार करके, Bash ने उपयोग में आसान, और पॉवरफुल स्ट्रिंग हैंडलिंग ऑप्शन मुहैया कराए। सबस्ट्रिंग में फ्लेक्सिबिलिटी है - आप ऑफसेट के साथ स्टार्ट कर सकते हैं, लेंथ स्पेसिफाई कर सकते हैं, या डिलिमिटर का उपयोग कर सकते हैं। इससे प्रोग्रामर्स को डेटा सैनिटाइजेशन, पार्सिंग, या कॉम्प्लेक्स लॉजिक के लिए उपयुक्त टूल्स मिलते हैं।

Awk और Perl जैसे अल्टरनेटिव टूल्स भी हैं जो पावरफुल टेक्स्ट प्रोसेसिंग प्रदान करते हैं, लेकिन सिंपल Bash स्क्रिप्टिंग अक्सर लाइटवेट और जल्दी काम चलाने के लिए काफी होती है। Bash के `{string:offset:length}` सिंटैक्स को समझना और कुशलता से उपयोग करना, यह शक्ति देता है Bash users को कि वे स्ट्रिंग मणिपुलेशन को सरलता से कर सकें।

## See Also (और भी देखें):
- Bash Substring Documentation: https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html
- Advanced Bash-Scripting Guide: https://tldp.org/LDP/abs/html/string-manipulation.html
- Stack Overflow Discussions: https://stackoverflow.com/questions/tagged/bash+substring

इन लिंक्स में आपको Bash सबस्ट्रिंग्स के और भी गहन जानकारी और उपयोगी टिप्स मिल जाएंगे।
