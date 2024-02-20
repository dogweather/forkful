---
date: 2024-01-20 17:46:36.430970-07:00
description: "\u0909\u092A\u0936\u092C\u094D\u0926 (substring) \u0928\u093F\u0915\u093E\
  \u0932\u0928\u093E \u092E\u0924\u0932\u092C \u0939\u0948 \u0915\u093F\u0938\u0940\
  \ \u0926\u093F\u090F \u0917\u090F \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\
  \ \u0938\u0947 \u0915\u0941\u091B \u0939\u093F\u0938\u094D\u0938\u093E \u0932\u0947\
  \u0928\u093E\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\
  \u0938 \u092F\u0939 \u0915\u093E\u092E \u0924\u092C \u0915\u0930\u0924\u0947 \u0939\
  \u0948\u0902 \u091C\u092C \u0909\u0928\u094D\u0939\u0947\u0902 \u092C\u0921\u093C\
  \u0940 \u0938\u0942\u091A\u0928\u093E \u092E\u0947\u0902 \u0938\u0947 \u0938\u093F\
  \u0930\u094D\u092B \u091C\u093C\u0930\u0942\u0930\u0940 \u0921\u0947\u091F\u093E\
  \u2026"
lastmod: 2024-02-19 22:05:10.628894
model: gpt-4-1106-preview
summary: "\u0909\u092A\u0936\u092C\u094D\u0926 (substring) \u0928\u093F\u0915\u093E\
  \u0932\u0928\u093E \u092E\u0924\u0932\u092C \u0939\u0948 \u0915\u093F\u0938\u0940\
  \ \u0926\u093F\u090F \u0917\u090F \u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\
  \ \u0938\u0947 \u0915\u0941\u091B \u0939\u093F\u0938\u094D\u0938\u093E \u0932\u0947\
  \u0928\u093E\u0964 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\
  \u0938 \u092F\u0939 \u0915\u093E\u092E \u0924\u092C \u0915\u0930\u0924\u0947 \u0939\
  \u0948\u0902 \u091C\u092C \u0909\u0928\u094D\u0939\u0947\u0902 \u092C\u0921\u093C\
  \u0940 \u0938\u0942\u091A\u0928\u093E \u092E\u0947\u0902 \u0938\u0947 \u0938\u093F\
  \u0930\u094D\u092B \u091C\u093C\u0930\u0942\u0930\u0940 \u0921\u0947\u091F\u093E\
  \u2026"
title: "\u0938\u092C\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917\u094D\u0938 \u0928\
  \u093F\u0915\u093E\u0932\u0928\u093E"
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
