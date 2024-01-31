---
title:                "रेगुलर एक्सप्रेशन का उपयोग"
date:                  2024-01-19
simple_title:         "रेगुलर एक्सप्रेशन का उपयोग"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/using-regular-expressions.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
रेग्युलर एक्सप्रेशन एक शक्तिशाली पैटर्न मैचिंग टूल है जो टेक्स्ट के ढेर में से खास जानकारी निकालता है। प्रोग्रामर्स इसका इस्तेमाल डेटा वैलिडेशन, सर्चिंग, और टेक्स्ट प्रोसेसिंग में करते हैं।

## How to: (कैसे करें:)
```Fish Shell
# सिंपल एक्साम्पल: 'fish' शब्द खोजें
echo "I love fishing in the Fish shell." | grep -o 'fish'

# आउटपुट: 'fish'

# केस सेंसिटिविटी को अनदेखा करें
echo "I love fishing in the Fish shell." | grep -oi 'fish'

# आउटपुट: 'fish'
#          'Fish'

# रेग्युलर एक्सप्रेशन का इस्तेमाल करके जो नंबर पाइप साइन के बाद आता है उसे ढूंढें
echo "My number is 987-654-3210." | grep -Eo '[0-9]+-[0-9]+-[0-9]+'

# आउटपुट: '987-654-3210'
```

## Deep Dive (गहराई से जानकारी)
- रेग्युलर एक्सप्रेशन की शुरुआत 1950 में हुई थी।
- प्रोग्रामर्स ग्रेप, सेड, और अवक जैसे अल्टरनेटिव्स भी इस्तेमाल करते हैं।
- Fish Shell में रेगुलर एक्सप्रेशन अन्य शेल्स की तरह इम्प्लीमेंट किए जाते हैं, लेकिन इसकी बिल्ट-इन कमांड्स कुछ भिन्न हो सकती हैं।

## See Also (देखें भी)
- [Fish Shell Documentation](https://fishshell.com/docs/current/index.html)
- [Regular Expressions in GNU grep](https://www.gnu.org/software/grep/manual/grep.html#Regular-Expressions)
- [Linux Regular Expression Tutorial](https://ryanstutorials.net/regular-expressions-tutorial/)
