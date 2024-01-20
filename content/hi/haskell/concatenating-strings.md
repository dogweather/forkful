---
title:                "स्ट्रिंग्स को जोड़ना"
html_title:           "Bash: स्ट्रिंग्स को जोड़ना"
simple_title:         "स्ट्रिंग्स को जोड़ना"
programming_language: "Haskell"
category:             "Haskell"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/haskell/concatenating-strings.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
(What and Why?)

स्ट्रिंग को कोन्कैटेनेट करने का मतलब होता है दो या दो से अधिक स्ट्रिंग को जोड़ना। प्रोग्रामर इसे सामग्री को एकत्र करने, पाठ्य स्वरूपण करने या डाटा को लोगों के लिए पठनीय बनाने के लिए करते हैं।

## कैसे करें:
(How to?)

हास्केल में, आप `(++)` ओपरेटर का उपयोग करके स्ट्रिंग को कोन्कैटेनेट कर सकते हैं।

```Haskell
Prelude> let str1 = "नमस्ते"
Prelude> let str2 = " दुनिया!"
Prelude> str1 ++ str2
"नमस्ते दुनिया!"
```

## गहरी जानकारी
(Deep Dive)

स्ट्रिंग को कोन्कैटेनेट करने का विचार लगभग कम्प्यूटर की कला के इतिहास के साथ समान रूप से पुराना है। इसके विभिन्न तरीके और कार्यक्रम भाषाएं उपलब्ध हैं। हास्केल में `(++)` ओपरेटर आपको दो स्ट्रिंग को सीधे जोड़ने की अनुमति देता है। यदि आपको किसी स्ट्रिंग के अंत में अनेक स्ट्रिंग्स जोड़ने हों, तो आप `concat` फ़ंक्शन का उपयोग कर सकते हैं।

## यह भी देखें
(See Also)

1. [Haskell दस्तावेज़ीकरण](https://docs.haskell-lang.org/)
2. [पाठ और स्ट्रिंग](https://haskell-lang.org/tutorial/string)
3. [हास्केल के `(++)` और `concat` कार्यक्रम](https://hoogle.haskell.org/?hoogle=%28++%29+or+concat)