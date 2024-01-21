---
title:                "सबस्ट्रिंग्स निकालना"
date:                  2024-01-20T17:46:36.430970-07:00
model:                 gpt-4-1106-preview
simple_title:         "सबस्ट्रिंग्स निकालना"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/extracting-substrings.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

उपशब्द (substring) निकालना मतलब है किसी दिए गए स्ट्रिंग से कुछ हिस्सा लेना। प्रोग्रामर्स यह काम तब करते हैं जब उन्हें बड़ी सूचना में से सिर्फ ज़रूरी डेटा चाहिए होता है।

## How to: (कैसे करें:)

Python में substrings को निकालने के लिए सबसे सीधा तरीका स्ट्रिंग slicing है:

```Python
# पूरी स्ट्रिंग
text = "Hello, World!"

# Substring इस्तेमाल करके पहले शब्द को निकालना
first_word = text[0:5]  # 'Hello'

# Substring इस्तेमाल करके आखिरी शब्द को निकालना
last_word = text[7:12]  # 'World'

print(first_word)  # Hello
print(last_word)   # World
```

इसके अलावा, `find()` और `index()` जैसे स्ट्रिंग मेथड्स का उपयोग कर विशेष स्थितियों में substring निकाल सकते हैं:

```Python
# 'World' शब्द की शुरुआती पोजिशन पता लगाकर substring निकालना
start_index = text.find('World')  # 7
if start_index != -1:
    world_substring = text[start_index:start_index+5]
    print(world_substring)  # World
```

## Deep Dive (गहराई में जानकारी)

स्ट्रिंग स्लाइसिंग Python की क्षमता है जो 1991 से ही उपयोग में है, जब Guido van Rossum ने Python को बनाया। इसका सैटेक्स सरल है: `s[start:stop:step]` जहाँ `start` स्टार्ट इंडेक्स है, `stop` एंड इंडेक्स है और `step` जंप की साइज है।

अगर `start` या `stop` में कोई संख्या नहीं दि जाए तो Python उसे समझता है और पूरी लेंथ लेता है। `step` में निगेटिव वैल्यू से उलटी स्लाइसिंग भी कर सकते हैं।

Alternatives और उनकी ताकत:
- `split()`: स्ट्रिंग को डिलिमिटर के हिसाब से बांटकर लिस्ट में रखता है।
- `partition()`: स्ट्रिंग को तीन हिस्सों में बांटता है, जो डिलिमिटर से पहले, डिलिमिटर खुद, और डिलिमिटर के बाद होते हैं।
- Regular Expressions (regex): जटिल पैटर्न मिलान और String manipulation के लिए।

## See Also (और भी देखें)

- Python के ऑफिशियल डॉक्युमेंटेशन में [String Methods](https://docs.python.org/3/library/stdtypes.html#string-methods)
- [Regex tutorial](https://www.regular-expressions.info/) - Regular Expressions सीखने के लिए comprehensive guide