---
title:                "स्ट्रिंग को कैपिटलाइज़ करना"
html_title:           "C: स्ट्रिंग को कैपिटलाइज़ करना"
simple_title:         "स्ट्रिंग को कैपिटलाइज़ करना"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

String capitalization मतलब हर शब्द के पहले अक्षर को बड़ा (Capital Letter) करना। Programmers इसे readability और user interfaces को अच्छा बनाने के लिए करते हैं।

## How to: (कैसे करें:)

Bash में string को capitalize करने का सीधा तरीका नहीं है, पर workarounds हैं। यहां दो examples हैं:

1. पूरी string के हर शब्द का पहला अक्षर बड़ा करना।

```Bash
#!/bin/bash
capitalize_string() {
  echo "$1" | awk '{for(i=1;i<=NF;i++) $i=toupper(substr($i,1,1)) substr($i,2)}1'
}

input="namaste duniya"
capitalized=$(capitalize_string "$input")
echo $capitalized
```

Sample Output:
```
Namaste Duniya
```

2. केवल पहले शब्द का पहला अक्षर बड़ा करना।

```Bash
#!/bin/bash
capitalize_first_word() {
  echo "$1" | sed 's/^\(.\)/\U\1/'
}

input="namaste duniya"
capitalized=$(capitalize_first_word "$input")
echo $capitalized
```

Sample Output:
```
Namaste duniya
```

## Deep Dive (गहराई से जानकारी):

Bash में string manipulation को directly support नहीं किया जाता जैसे कि कुछ high-level programming languages में होता है। इसे script में शामिल करने के लिए awk और sed जैसे tools का इस्तेमाल करते हैं। awk powerful text-processing language है जो complex pattern recognition और processing को संभव बनाता हैं। sed, यानी stream editor, भी text manipulation के लिए उपयोग में लिया जाता है।

ऐतिहासिक रूप से, text processing के लिए dedicated utilities का उपयोग करने का प्रचलन इसलिए है क्योंकि original Unix philosophy में छोटे और विशेषीकृत प्रोग्राम्स का महत्व था जो एक साथ पाइप किये जा सकते हैं।

इन-built string manipulation capabilities, जैसे कि `bash parameter expansion`, भी हैं, पर वे capitalization के लिए नहीं हैं।

## See Also (और देखें):

- [GNU Awk User's Guide](https://www.gnu.org/software/gawk/manual/gawk.html)
- [GNU sed Manual](https://www.gnu.org/software/sed/manual/sed.html)
- [Bash Parameter Expansion](https://www.gnu.org/software/bash/manual/html_node/Shell-Parameter-Expansion.html)
