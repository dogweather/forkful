---
title:                "स्ट्रिंग को कैपिटलाइज़ करना"
html_title:           "C: स्ट्रिंग को कैपिटलाइज़ करना"
simple_title:         "स्ट्रिंग को कैपिटलाइज़ करना"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
स्ट्रिंग को कैपिटलाइज़ करना मतलब हर वर्ड के पहले अक्षर को बड़ा करना होता है। प्रोग्रामर इसे डेटा को मानक स्वरूप देने के लिए और पठनीयता बढ़ाने के लिए करते हैं।

## कैसे करें:
जानिए एक स्ट्रिंग को कैसे कैपिटलाइज़ करें:

```Python
# .title() का उपयोग करके
text = "मेरा नाम अजय है"
capitalized_text = text.title()
print(capitalized_text)

# Output: मेरा नाम अजय है
```

```Python
# स्ट्रिंग मेथड्स का कॉम्बिनेशन
def capitalize_words(string):
    return ' '.join(word.capitalize() for word in string.split())

example_text = "मेरा नाम अजय है"
print(capitalize_words(example_text))

# Output: मेरा नाम अजय है
```

## गहराई से जानकारी:
स्ट्रिंग्स को कैपिटलाइज़ करना बहुत साधारण परंतु उपयोगी कार्य है। `.title()` पायथन की बिल्ट-इन फंक्शन है, जो 1980 के दशक में पायथन की शुरुआत के समय से ही है। यह फंक्शन हर शब्द के पहले अक्षर को बड़ा कर देता है परंतु इसमें कुछ सीमायें हैं, जैसे कि यह सिर्फ अल्फान्युमैरिक शब्दों के लिए काम करता है और नॉन-लैटिन अक्षरों पे भिन्न परिणाम हो सकता है।

इसलिए अक्सर प्रोग्रामर अपने कस्टम फंक्शन लिखते हैं जैसे कि `capitalize_words`. यह फ़ंक्शन `.split()` से स्ट्रिंग को शब्दों में बांटता है, फिर `.capitalize()` से हर शब्द को कैपिटलाइज़ करता है, और अंत में `' '.join()` से सबको वापस एक साथ जोड़ता है।

## देखिये भी:
- [Python’s str.capitalize() method](https://docs.python.org/3/library/stdtypes.html#str.capitalize)
- [Python’s str.title() method](https://docs.python.org/3/library/stdtypes.html#str.title)
- [Python’s Official Documentation](https://docs.python.org/3/)
