---
title:                "स्ट्रिंग को कैपिटलाइज़ करना"
html_title:           "C: स्ट्रिंग को कैपिटलाइज़ करना"
simple_title:         "स्ट्रिंग को कैपिटलाइज़ करना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
स्ट्रिंग कैपिटलाइज़िशन यानी कि स्ट्रिंग के पहले अक्षर को बड़ा (कैपिटल) बनाना. प्रोग्रामर्स इसे तब इस्तेमाल करते हैं जब वो पाठ को फॉर्मलाइज़ करना चाहते हैं, जैसे कि नामों या शीर्षकों में.

## How to: (कैसे करें)
Fish Shell में स्ट्रिंग कैपिटलाइज़ करने के लिए चलिए कुछ उदाहरण देखते हैं:

```Fish Shell
# पहले अक्षर को कैपिटल करने के लिए कमांड
function capitalize
    echo $argv[1] | awk '{ print toupper(substr($0, 1, 1)) substr($0, 2) }'
end

# फंक्शन का प्रयोग
capitalize "namaste duniya"
```

आउटपुट:
```
Namaste duniya
```

## Deep Dive (गहराई में जानकारी)
स्ट्रिंग कैपिटलाइज़ करने की प्रक्रिया पुरानी है और कंप्यूटिंग के शुरुआती दिनों से ही प्रचलन में है. Fish Shell में `awk` उपयोग करके हम आसानी से पहले अक्षर को कैपिटल कर सकते हैं लेकिन ये केवल एक रास्ता है. अन्य अल्टरनेटिव्स में `sed`, `perl`, या `tr` जैसे टूल्स भी शामिल हैं जिनका उपयोग कस्टम फंक्शंस में हो सकता है. इस प्रक्रिया का मुख्य हिस्सा पहले अक्षर को पहचानना और उसे उच्चक (uppercase) में परिवर्तित करना है, जिसे `awk` के `toupper` फंक्शन से किया जा सकता है.

## See Also (अन्य संसाधन)
- Fish Shell के आधिकारिक दस्तावेज़: [Fish Documentation](https://fishshell.com/docs/current/index.html)
- `awk` कमांड का प्रयोग: [AWK Manual](https://www.gnu.org/software/gawk/manual/gawk.html)
- स्ट्रिंग मैनिपुलेशन और शैल स्क्रिप्टिंग में सहायता के लिए: [Shell Scripting Tutorial](https://www.shellscript.sh/)
