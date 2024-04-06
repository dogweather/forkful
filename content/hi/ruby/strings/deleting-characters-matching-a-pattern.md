---
date: 2024-01-20 17:43:30.167412-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) ."
lastmod: '2024-04-05T21:53:55.156634-06:00'
model: gpt-4-1106-preview
summary: ''
title: "\u092A\u0948\u091F\u0930\u094D\u0928 \u0938\u0947 \u092E\u0947\u0932 \u0916\
  \u093E\u0924\u0947 \u0905\u0915\u094D\u0937\u0930\u094B\u0902 \u0915\u094B \u0939\
  \u091F\u093E\u0928\u093E"
weight: 5
---

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
