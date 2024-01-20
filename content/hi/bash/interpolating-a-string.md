---
title:                "स्ट्रिंग का अंतर्कलन"
html_title:           "Arduino: स्ट्रिंग का अंतर्कलन"
simple_title:         "स्ट्रिंग का अंतर्कलन"
programming_language: "Bash"
category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/interpolating-a-string.md"
---

{{< edit_this_page >}}

## क्या & क्यों?

String interpolations एक भाषा feature है जिसका उपयोग variables या expressions के मूल्यों को एक string में embed करने के लिए किया जाता है। प्रोग्रामर्स इसका उपयोग तब करते हैं जब उन्हें string के आउटपुट में चर के मूल्य को शामिल करना होता है।

## कैसे करें
 
```Bash
# एक variable तैयार करें
name="विशाल"

# इसे string में interpolate करें
echo "नमस्ते, ${name}!"

# आउटपुट: नमस्ते, विशाल!
```

## गहरी जानकारी

1. हिस्टोरिकल कन्टेक्स्ट: String interpolation का उपण्यास, Perl और Ruby जैसी भाषाओं में आम है, लेकिन Bash ने इसे अपने अपने स्वरूप में अपनाया है।

2. विकल्प: अगर किसी वेब पैठ या कमांड के लिए string interpolation की आवश्यकता नहीं हो, तो concatenation सबसे साधारण विकल्प हो सकता है। 

3. आवश्यक विवरण: Bash में, आप `${variable}` syntax का उपयोग कर string में variable interpolate कर सकते हैं।

## अन्य स्रोतों के लिंक

1. GNU Bash मैनुअल: https://www.gnu.org/software/bash/manual/bash.html
2. Advanced Bash-Scripting Guide: https://tldp.org/LDP/abs/html/string-manipulation.html
3. Bash String Manipulations: https://linuxconfig.org/bash-string-manipulation