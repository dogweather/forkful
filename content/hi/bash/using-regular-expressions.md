---
title:                "रेगुलर एक्सप्रेशन का उपयोग"
date:                  2024-01-19
html_title:           "Bash: रेगुलर एक्सप्रेशन का उपयोग"
simple_title:         "रेगुलर एक्सप्रेशन का उपयोग"

category:             "Bash"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/bash/using-regular-expressions.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
रेगुलर एक्सप्रेशन्स यानी नियमित अभिव्यक्तियाँ, पैटर्न की एक ऐसी भाषा है, जिसका इस्तेमाल हम टेक्स्ट को खोजने, बदलने और प्रबंधित करने के लिए करते हैं। प्रोग्रामर्स इसे इसलिए इस्तेमाल करते हैं क्योंकि ये जटिल टेक्स्ट ऑपरेशन्स को सरल और तेज़ बनाते हैं।

## कैसे करें? (How to:)
```Bash
# फ़ाइलों की सूची में से .txt फ़ाइलों को खोजें
ls | grep '\.txt$'

# सैम्पल आउटपुट
document.txt
notes.txt
report.txt

# फ़ाइल के अंदर से "error" शब्द को ढूँढना
grep 'error' server.log

# सैम्पल आउटपुट
error: file not found
error: connection lost
```

## गहराई में (Deep Dive)
रेगुलर एक्सप्रेशंस की शुरुआत 1950 के दशक में हुई थी और केन थॉम्पसन ने Unix के लिए पहले एडिटर 'ed' में इसका इस्तेमाल किया था। पर्याय: आप egrep, awk या perl जैसे उपकरणों का इस्तेमाल कर सकते हैं। नियमित अभिव्यक्तियाँ दो प्रकार की होती हैं: बेसिक और एक्सटेंडेड। Bash में, grep बेसिक प्रकार का इस्तेमाल करता है पर egrep और awk एक्सटेंडेड रेगुलर एक्सप्रेशंस का उपयोग करते हैं।

## और जानें (See Also)
- GNU Grep मैनुअल: https://www.gnu.org/software/grep/manual/grep.html
- Regular-Expressions.info: https://www.regular-expressions.info/
- Online regex tester and debugger: https://regex101.com/
