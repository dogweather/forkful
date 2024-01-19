---
title:                "पाठ की खोज और प्रतिस्थापन"
html_title:           "Bash: पाठ की खोज और प्रतिस्थापन"
simple_title:         "पाठ की खोज और प्रतिस्थापन"
programming_language: "Clojure"
category:             "Clojure"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/clojure/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
खोजना और बदलना शब्दों या संकेतों को किसी पाठ में से मिलाने और उन्हें नए पाठ से प्रतिस्थापित करने का कार्य होता है। प्रोग्रामर्स इसे विभिन्न कार्यों, जैसे की डाटा संसाधन, कोड अपडेट करने, और डिबगिंग के लिए उपयोग करते हैं।

## कैसे करें:
यहाँ कुछ क्लोजर कोड के उदाहरण हैं जो खोजने और प्रतिस्थापन के लिए कैसे कार्य करते हैं। 

```Clojure
; सीधे txt में खोजने और प्रतिस्थापित करने का कोड
(defn replace-text [txt old new]
  (clojure.string/replace txt old new))

(replace-text "नमस्ते, दुनिया!" "दुनिया" "विश्व")
```
आउटपुट:  
```Clojure
"नमस्ते, विश्व!"
```

## गहरा डाइव:
खोजने और प्रतिस्थापित करने में क्लोजर 'clojure.string/replace' का उपयोग करने की जानकारी 1950s के ANSI standard के कारण है। याद रखें, यही 'replace' कार्य विभिन्न प्लेटफ़ॉर्म और पाठों प्रकारों पर अलग तरह से काम कर सकता है।

विकल्प साँचों का उपयोग करके कस्टम खोज और प्रतिस्थापन करने का हो सकता है, जैसे `clojure.string/replace-re` के साथ। 

## देखें भी:
1. Clojure की आधिकारिक डॉक्युमेन्टेशन: https://clojuredocs.org/clojure.string/replace
2. एंसी स्टैंडर्ड: https://www.ansi.org/
3. Clojure ट्यूटोरियल: https://www.learn-clojure.com/