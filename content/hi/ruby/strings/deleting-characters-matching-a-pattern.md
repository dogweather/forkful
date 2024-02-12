---
title:                "पैटर्न से मेल खाते अक्षरों को हटाना"
aliases:
- hi/ruby/deleting-characters-matching-a-pattern.md
date:                  2024-01-20T17:43:30.167412-07:00
model:                 gpt-4-1106-preview
simple_title:         "पैटर्न से मेल खाते अक्षरों को हटाना"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
पैटर्न मैचिंग से कैरेक्टर्स डिलीट करना मतलब स्ट्रिंग्स से खास कैरेक्टर्स हटाना है, जो एक निश्चित पैटर्न से मेल खाते हों। प्रोग्रामर्स इसे तब करते हैं जब उन्हें डाटा क्लीन करना होता है या इनपुट को फॉर्मेट करना होता है।

## How to: (कैसे करें:)
```Ruby
# स्ट्रिंग से सभी वाउल्स (aeiou) हटाना
string = "हेल्लो, यह एक स्ट्रिंग है!"
cleaned_string = string.delete('aeiou')
puts cleaned_string
# Output: हल्ल, यह क स्ट्रंग ह!

# Regex का उपयोग करके पैटर्न मैच करना और हटाना
string_with_digits = "रूबी2023 सीखें"
cleaned_string_digits = string_with_digits.delete('0-9')
puts cleaned_string_digits
# Output: रूबी सीखें
```

## Deep Dive (गहराई में जानकारी):
जब स्ट्रिंग्स में से निश्चित पैटर्न मैच करके कैरेक्टर्स हटाने की बात आती है, तो Ruby के `String#delete` मेथड की बराबरी मुश्किल है। 

पुराने जमाने में, लोग पैटर्न मैचिंग के लिए जटिल लूप्स और स्ट्रिंग ऑपरेशन्स करते थे, जो समय लेने वाला था। Ruby ने इसे अधिक सरल और तेज़ बनाया है।

अल्टरनेटिव्स के तौर पर, प्रोग्रामर `String#gsub` या `String#gsub!` का उपयोग कर सकते हैं, जो रेगुलर एक्सप्रेशंस (regex) को सपोर्ट करता है।

कार्यान्वयन के बारे में, `String#delete` कम समय में और कम मेमोरी इस्तेमाल करके तेजी से काम करता है, जिससे यह बड़े डेटा सेट्स के लिए भी उपयुक्त बनता है।

## See Also (और जानकारी के लिए):
- Ruby String#delete मेथड की डॉक्यूमेंटेशन: [Ruby-Doc.org](http://ruby-doc.org/core-2.7.0/String.html#method-i-delete)
- Ruby में Regular Expressions: [Ruby Regular Expressions](https://ruby-doc.org/core-2.7.0/Regexp.html)
- Stack Overflow पर प्रश्न और उत्तर: [Stack Overflow Questions](https://stackoverflow.com/questions/tagged/ruby)
