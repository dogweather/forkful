---
title:                "पाठ की खोज और प्रतिस्थापन"
html_title:           "Bash: पाठ की खोज और प्रतिस्थापन"
simple_title:         "पाठ की खोज और प्रतिस्थापन"
programming_language: "Ruby"
category:             "Ruby"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/ruby/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

# खोजे और बदलें: निबंध के रूप में Ruby प्रोग्रामिंग 

## क्या और क्यों?
"खोजें और बदलें" एक बड़ी स्ट्रिंग से विशेष टेक्स्ट स्ट्रिंग को खोजना और बदलना होता है। यह प्रोग्रामर्स को कोड के विभिन्न हिस्सों को बदलने में मदद करता है। 

## कैसे करें:
Ruby में "ग्लोबल सब्स्टिट्यूट" उपयोग करके खोजें और बदलें। 

```Ruby
text = "नमस्ते दुनिया!"
text.gsub!('नमस्ते', 'हेल्लो')
puts text
# Output: "हेल्लो दुनिया!"
```
'gsub!' मेथड शब्द 'नमस्ते' को 'हेल्लो' से बदल देगा।

## गहरी चर्चा:

1. ऐतिहासिक संदर्भ: "खोजें और बदलें" ऑपरेशन पुराने टेक्स्ट एडिटिंग और कम्पाइलर उपकरणों से स्रोत करता है। रूबी में इसे 'gsub' और 'sub' मेथड्स के माध्यम से कार्यान्वित किया जाता है। 

2. विकल्प: रूबी में, सिंगल रिप्लेसमेंट के लिए 'sub!' का उपयोग किया जा सकता है। 

3. कार्यान्वयन विवरण: 'gsub!'  हर ओकरेंस को बदलने के लिए काम करता है। यदि केवल पहले मिलने वाले शब्द को बदलना है, 'sub!'  का उपयोग करें। 

## देखें भी:

1. [Ruby Doc on gsub](https://ruby-doc.org/core-2.7.1/String.html#method-i-gsub)


3. [StackOverflow on gsub vs sub](https://stackoverflow.com/questions/19445003/using-ruby-what-does-sub-and-gsub-mean)