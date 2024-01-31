---
title:                "स्ट्रिंग का अंतर्कलन"
date:                  2024-01-28T21:24:20.922723-07:00
model:                 gpt-4-0125-preview
simple_title:         "स्ट्रिंग का अंतर्कलन"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/interpolating-a-string.md"
changelog:
  - 2024-01-28, dogweather, reviewed
  - 2024-01-28, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
स्ट्रिंग इंटरपोलेशन स्ट्रिंग लिटरल्स के भीतर एक्सप्रेशन्स को एम्बेड करने की विधि है। प्रोग्रामर इसका उपयोग डायनामिक रूप से स्ट्रिंग्स में मानों को सम्मिलित करने के लिए करते हैं, जो कोड को पारंपरिक स्ट्रिंग संगठन की तुलना में अधिक पठनीय और साफ बनाता है।

## कैसे:
Python 3.6 और इसके बाद के संस्करणों में, आप f-strings का उपयोग करके स्ट्रिंग्स को इंटरपोलेट कर सकते हैं। यहाँ पर कैसे करें:

```Python
name = 'Alice'
age = 30
greeting = f"नमस्ते, {name}. आप {age} वर्ष के हैं।"

print(greeting)
```

आउटपुट:
```
नमस्ते, Alice. आप 30 वर्ष के हैं।
```

आप कर्ली ब्रेसिज़ के अंदर एक्सप्रेशन्स का भी उपयोग कर सकते हैं:

```Python
a = 5
b = 10
info = f"पाँच प्लस दस {a + b} है, न कि {2 * (a + b)}।"

print(info)
```

आउटपुट:
```
पाँच प्लस दस 15 है, न कि 30।
```

## गहराई से:
Python 3.6 से पहले, `.format()` स्ट्रिंग्स को इंटरपोलेट करने का तरीका था:

```Python
name = 'Bob'
age = 25
greeting = "नमस्ते, {}. आप {} वर्ष के हैं।".format(name, age)

print(greeting)
```

पुराने स्कूल Python (संस्करण < 2.6) में इंटरपोलेशन के लिए `%` ऑपरेटर का उपयोग किया जाता था, जो कम सहज है और एकाधिक वेरिएबल के साथ अव्यवस्थित हो सकता है:

```Python
name = 'Carol'
age = 35
greeting = "नमस्ते, %s. आप %d वर्ष के हैं।" % (name, age)

print(greeting)
```

साफ सिंटैक्स के अलावा, f-strings तेज होते हैं क्योंकि वे रनटाइम पर मूल्यांकित होते हैं और फिर सीधे एक कुशल स्ट्रिंग फॉरमेट ऑपरेशन में परिवर्तित हो जाते हैं। `.format()` और `%` ऑपरेटर अधिक चरणों में शामिल होते हैं और धीमे होते हैं।

## देखें भी
- [PEP 498 – लिटरल स्ट्रिंग इंटरपोलेशन](https://www.python.org/dev/peps/pep-0498/) f-strings पर आधिकारिक दस्तावेज़ीकरण के लिए।
- [Python f-strings](https://realpython.com/python-f-strings/) f-strings का उपयोग करने पर रियल Python द्वारा एक ट्यूटोरियल।
- [The .format() Method](https://docs.python.org/3/library/stdtypes.html#str.format) पुराने `.format()` स्ट्रिंग फॉर्मेटिंग विधि को समझने के लिए Python दस्तावेज़ीकरण में।
