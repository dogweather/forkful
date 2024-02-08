---
title:                "स्ट्रिंग को जोड़ना"
date:                  2024-01-20T17:34:16.730481-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग को जोड़ना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/concatenating-strings.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
जब हम एक के बाद एक स्ट्रिंग्स को जोड़ते हैं, इसे स्ट्रिंग जोड़ना कहते हैं। इसका उपयोग डेटा को संगठित करने, मेसेजेस बनाने, और डायनैमिक आउटपुट तैयार करने में होता है।

## How to: (कैसे करें:)
```Bash
# स्ट्रिंग्स जोड़ना बहुत सरल है
name="राज"
greeting="नमस्ते, $name!"
echo $greeting  # नमस्ते, राज!

# कर्ली ब्रेसेस का उपयोग करके
welcome_message="आपका स्वागत है"
user="विजय"
echo "${welcome_message}, ${user}!"  # आपका स्वागत है, विजय!
```

## Deep Dive (गहराई में):
स्ट्रिंग जोड़ना शुरुआती UNIX शेल्स के समय से एक मूलभूत फीचर है। Bash में, यह बहुत कुशल है क्योंकि यह किसी अतिरिक्त प्रोसेस की जरूरत नहीं है। वैकल्पिक तरीके के तौर पर, `paste` और `awk` जैसे कमांड्स का उपयोग कर सकते हैं, लेकिन सामान्यतया उनका उपयोग फाइलों के साथ होता है, न कि साधारण वेरिएबल के साथ।

किसी भी कॉम्प्लेक्स आपरेशन के लिए जहां बहुत सारी स्ट्रिंग मैनिपुलेशन की जरूरत होती है, अक्सर यह बेहतर होता है कि हम Perl या Python जैसी स्क्रिप्टिंग भाषाओं का प्रयोग करें। Bash में, जोड़ना केवल वेरिएबल के नाम के बीच में स्पेस न छोड़कर उन्हें एक साथ लिखकर किया जा सकता है, जिसे कोटेशन मार्क्स और ब्रेसेस के साथ और भी स्पष्ट किया जा सकता है।

## See Also (और भी देखें):
- [GNU Bash documentation](https://www.gnu.org/software/bash/manual/bash.html)
- [Advanced Bash-Scripting Guide](https://www.tldp.org/LDP/abs/html/)
- [Bash String Manipulation Examples](https://linuxize.com/post/bash-concatenate-strings/)
