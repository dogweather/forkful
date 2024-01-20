---
title:                "सबस्ट्रिंग्स निकालना"
html_title:           "Clojure: सबस्ट्रिंग्स निकालना"
simple_title:         "सबस्ट्रिंग्स निकालना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/extracting-substrings.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

Substring उत्खनन एक programming तकनीक है, जहाँ हम एक लम्बे स्ट्रिंग को छोटे भागों में विभाजित करते हैं। Programmers इसे उन संदेशों को पाठ्य रूप में पर्स करने के लिए उपयोग करते हैं, जिन्हें उपयोगकर्ता ने दर्ज किया है, या वेबपेजेस से डाटा खींचने के लिए।

## कैसे करें:

नीचे Fish Shell में substring उत्खनन का उदाहरण दिया गया है।

```Fish Shell
set string "हैलो, वर्ल्ड"
echo $string[1..5]
```

ऊपरी कोड अनुभाग "हैलो" को प्रिंट करेगा। यहां, '[1..5]' एक range operator है जो पहले पांच characters को चुनता है।

## गहरी डाइव:

Fish Shell, जिसे 2005 में रिलीज़ किया गया था, का उद्देश्य उपयोगकर्ता प्रवेश को सरल और स्पष्ट बनाना है। यह बाकी स्क्रिप्टिंग भाषाओं से विभिन्न है जिनमे substring उत्खनन को छोटे और अधिक कम्पलेक्स कोड्स में किया जा सकता है।

Bash और Zsh जैसी अन्य शेल्स से तुलना में, Fish अपनी सरलता के लिए लोकप्रिय है, लेकिन इसकी कम्पटिबलिटी इन अन्य शेल्स के मुकाबले कुछ कम हो सकती है। 

## देखें भी:

- Fish Shell डॉक्यूमेंटेशन: [https://fishshell.com/docs/current/index.html](https://fishshell.com/docs/current/index.html)
- Substring Extraction में विभिन्न Programming Languages की तुलना: [https://www.programiz.com/article/substring-different-languages](https://www.programiz.com/article/substring-different-languages)