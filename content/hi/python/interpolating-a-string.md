---
title:                "स्ट्रिंग इंटरपोलेशन"
date:                  2024-01-20T17:51:37.194500-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग इंटरपोलेशन"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/interpolating-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)

स्ट्रिंग इंटरपोलेशन यानी वेरिएबल और एक्सप्रेशंस को स्ट्रिंग के अंदर डायरेक्ट डालना। प्रोग्रामर इसे इस्तेमाल करते हैं ताकि कोड साफ़, समझने में आसान और डायनामिक स्ट्रिंग बनाने में मदद मिल सके।

## कैसे करें? (How to:)

```python
# f-strings के इस्तेमाल से स्ट्रिंग इंटरपोलेशन
name = "नेहा"
age = 30
greeting = f"नमस्ते, मेरा नाम {name} है और मैं {age} साल की हूँ।"
print(greeting)
```

आउटपुट:
```
नमस्ते, मेरा नाम नेहा है और मैं 30 साल की हूँ।
```

## गहराई से समझ (Deep Dive)

स्ट्रिंग इंटरपोलेशन का इतिहास: पहले प्रोग्रामर स्ट्रिंग फॉर्मेटिंग के लिए `%` ऑपरेटर या `format()` फंक्शन का उपयोग करते थे। फिर, Python 3.6 में फॉर्मेटेड स्ट्रिंग लिटरल्स या f-strings पेश किए गए, जो ज्यादा पठनीय और कुशल हैं।

कुछ विकल्प इस प्रकार हैं:
```python
# % ऑपरेटर का उपयोग करके
greeting = "नमस्ते, मेरा नाम %s है और मैं %d साल की हूँ।" % (name, age)

# format() फंक्शन का इस्तेमाल करके
greeting = "नमस्ते, मेरा नाम {} है और मैं {} साल की हूँ।".format(name, age)
```

कार्यप्रणाली: जब `f-string` का इस्तेमाल होता है, पायथन रन-टाइम पर एक्सप्रेशन को इवैल्युएट करता है और उन्हें स्ट्रिंग में बदल देता है, जिससे यह मेमोरी मैनेजमेंट में भी कुशल होता है।

## देखें भी (See Also)

- Python 3.8 डोक्यूमेंटेशन: f-Strings: https://docs.python.org/3/reference/lexical_analysis.html#f-strings
- PEP 498 – Literal String Interpolation: https://www.python.org/dev/peps/pep-0498/
- Real Python गाइड – Python String Formatting Best Practices: https://realpython.com/python-string-formatting/
