---
title:                "स्ट्रिंग को लोअर केस में बदलना"
html_title:           "Fish Shell: स्ट्रिंग को लोअर केस में बदलना"
simple_title:         "स्ट्रिंग को लोअर केस में बदलना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

String को lower case में convert करना एक programming technique है, जिससे हमें string के साथ और आसान तरीके से काम करने में मदद मिलती है। Programmers कई बार strings को search करने और उन्हें case-insensitive तरीके से compare करने के लिए lower case में convert करते हैं।

## कैसे करें?

```Fish Shell``` का उपयोग करके string को lower case में convert करने के लिए आपको ```string tolower``` command का उपयोग करना होगा। निम्नलिखित उदाहरण देखें:
```
string tolower "HELLO EVERYONE" # output: hello everyone
string tolower "MaGiC sTrInG" #output: magic string
```

## गहराई में जाएं

(1) Lower case string convert करने की technique 1970s से प्रभावी तरीके से उपयोग की जा रही है। (2) इसके अलावा, आप ```string tolower``` के स्थान पर अन्य programming languages के माध्यम से भी string को lower case में कन्वर्ट कर सकते हैं। (3) ```string tolower``` का अंतिम परिणाम एंक्रिप्शन के लिए एक शकल रूप देता है, जो डाटा सुरक्षित रूप से रखने में मदद करता है।

## देखें

यदि आपको और गहराई की जानकारी चाहिए तो आप निम्नलिखित से संबद्ध स्रोतों को देख सकते हैं:
- [Fish Shell documentation](https://fishshell.com/docs/current/)
- [GeeksforGeeks article on string to lower case](https://www.geeksforgeeks.org/string-tolower-in-cpp/)
- [Stack Overflow thread on alternatives to string to lower case](https://stackoverflow.com/questions/2139856/string-to-lower-in-c)