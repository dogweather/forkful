---
title:                "स्ट्रिंग को लोअर केस में बदलना"
html_title:           "Bash: स्ट्रिंग को लोअर केस में बदलना"
simple_title:         "स्ट्रिंग को लोअर केस में बदलना"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
एक बात बड़ी सरल है - प्रोग्रामर्स किसी भी स्ट्रिंग को छोटे अक्षर में बदलने से उसकी वैधता बनी रहती है और उसकी प्रोग्रामिंग में आसानी हो जाती है। छोटा करने के लिए, प्रोग्रामर्स बाश (Bash) कमांड का उपयोग कर सकते हैं।

## कैसे करें?
आइये एक स्ट्रिंग को छोटे अक्षर में करने के दो तरीकों को देखें।
```Bash
# तरीका 1:
echo "HELLO WORLD" | tr '[:upper:]' '[:lower:]'

# तरीका 2:
string="HELLO WORLD"
echo "${string,,}"
```
आउटपुट:
```Bash
hello world
```

## गहराई में जाएं
इतिहास से संबन्धित - पूर्व के ऑपरेटिंग सिस्टम एंड प्रोग्रामिंग में, स्ट्रिंग ऑपरेटिंग की बात जरूर होती थी। जो अपनी जगह से उतरते जा रहे हैं, लेकिन बाश (Bash) स्ट्रिंग छोटे करने के लिए शक्तिशाली है और बहुत कुछ और भी कर सकता है। उसके अलावा, आप स्ट्रिंग को आसानी से हेक् कर सकते हैं। जैसे की ऊपर विस्तार से दिखाया गया है।

## और भी देखें
- पूरा बाश (Bash) गाइड: https://www.gnu.org/software/bash/manual/bash.html
- सिस्टम में छोटे अक्षर कैसे करें: https://www.oreilly.com/library/view/linux-command-line/0596005954/re74.html