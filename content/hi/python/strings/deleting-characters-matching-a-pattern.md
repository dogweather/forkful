---
changelog:
- 2024-04-04, dogweather, edited
- 2024-04-04, gpt-4-0125-preview, translated from English
date: 2024-01-20 17:43:02.363431-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: ."
lastmod: '2024-04-04T02:03:20.355513-06:00'
model: gpt-4-0125-preview
summary: .
title: "\u090F\u0915 \u092A\u0948\u091F\u0930\u094D\u0928 \u0938\u0947 \u092E\u0947\
  \u0932 \u0915\u0930\u0928\u0947 \u0935\u093E\u0932\u0947 \u0935\u0930\u094D\u0923\
  \u094B\u0902 \u0915\u094B \u0939\u091F\u093E\u0928\u093E"
weight: 5
---

## कैसे करें:
```Python
import re

# उदाहरण स्ट्रिंग
text = "Hello, World! 1234"

# सभी अंकों को हटाएं
no_digits = re.sub(r'\d', '', text)
print(no_digits)  # आउटपुट: "Hello, World! "

# विराम चिह्न हटाएं
no_punctuation = re.sub(r'[^\w\s]', '', text)
print(no_punctuation)  # आउटपुट: "Hello World 1234"

# स्वर वर्ण हटाएं
no_vowels = re.sub(r'[aeiouAEIOU]', '', text)
print(no_vowels)  # आउटपुट: "Hll, Wrld! 1234"
```

### मेरा कस्टम फंक्शन

मैं यह कार्य पर्याप्त बार करता हूं जिसके लिए मैंने इस सरल `delete()` फंक्शन को पुनर्गठित किया है। यह [doctests](https://docs.python.org/3/library/doctest.html) का एक अच्छा दर्शन भी है:

```python
def delete(string: str, regex: str) -> str:
    """
    >>> delete("Hello, world!", "l")
    'Heo, word!'

    >>> delete("Hello, world!", "[a-z]")
    'H, !'
    """
    return re.sub(regex, "", string)
```

## गहराई से जानकारी
किसी पैटर्न से मेल खाने वाले अक्षरों को पाठ में से हटाने का अभ्यास कंप्यूटर विज्ञान में गहराई से निहित है, जो `sed` और `grep` जैसे प्रारंभिक Unix उपकरणों तक पहुँचता है। Python में, `re` मॉड्यूल यह क्षमता प्रदान करता है, नियमित अभिव्यक्तियों का उपयोग करके— एक शक्तिशाली और बहुमुखी पाठ संसाधन उपकरण।

`re` मॉड्यूल के विकल्पों में शामिल हैं:
- साधारण मामलों के लिए `replace()` जैसी स्ट्रिंग विधियाँ।
- जटिल पैटर्न और बेहतर Unicode समर्थन के लिए `regex` जैसे तृतीय-पक्ष पुस्तकालय। 

आंतरिक रूप से, जब आप `re.sub()` का उपयोग करते हैं, तो Python व्याख्यानी बाइटकोड के एक सिरीज़ में पैटर्न को संकलित करता है, जिसे एक राज्य मशीन द्वारा संसाधित किया जाता है जो प्रवेश पाठ पर सीधे पैटर्न-मिलान प्रदर्शित करता है। बड़ी स्ट्रिंग्स या जटिल पैटर्न के लिए यह संचालन संसाधन-गहन हो सकता है, इसलिए बड़े डेटा प्रसंस्करण के लिए प्रदर्शन पर विचार करना महत्वपूर्ण है। 

## देखें
- [Python `re` मॉड्यूल दस्तावेज़](https://docs.python.org/3/library/re.html): Python में नियमित अभिव्यक्तियों के लिए आधिकारिक दस्तावेज़।
- [Regular-Expressions.info](https://www.regular-expressions.info/): नियमित अभिव्यक्तियों के लिए एक व्यापक मार्गदर्शिका।
- [Real Python ट्यूटोरियल ऑन रेगेक्स](https://realpython.com/regex-python/): Python में नियमित अभिव्यक्तियों के वास्तविक दुनिया के अनुप्रयोग।
