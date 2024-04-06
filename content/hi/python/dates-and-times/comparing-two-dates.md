---
date: 2024-01-20 17:34:05.528925-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902? (How to:) Python\
  \ \u092E\u0947\u0902 \u0926\u094B \u0924\u093E\u0930\u0940\u0916\u094B\u0902 \u0915\
  \u0940 \u0924\u0941\u0932\u0928\u093E `datetime` \u092E\u0949\u0921\u094D\u092F\u0942\
  \u0932 \u0938\u0947 \u0915\u0940 \u091C\u093E \u0938\u0915\u0924\u0940 \u0939\u0948\
  \u0964 1990 \u092E\u0947\u0902 Python \u0915\u0940 \u0936\u0941\u0930\u0941\u0906\
  \u0924 \u0938\u0947, `datetime` \u092E\u093E\u0928\u0915 \u092A\u0941\u0938\u094D\
  \u0924\u0915\u093E\u0932\u092F \u092E\u0947\u0902 \u090F\u0915\u2026"
lastmod: '2024-04-05T22:51:06.302998-06:00'
model: gpt-4-1106-preview
summary: ") Python \u092E\u0947\u0902 \u0926\u094B \u0924\u093E\u0930\u0940\u0916\u094B\
  \u0902 \u0915\u0940 \u0924\u0941\u0932\u0928\u093E `datetime` \u092E\u0949\u0921\
  \u094D\u092F\u0942\u0932 \u0938\u0947 \u0915\u0940 \u091C\u093E \u0938\u0915\u0924\
  \u0940 \u0939\u0948\u0964 1990 \u092E\u0947\u0902 Python \u0915\u0940 \u0936\u0941\
  \u0930\u0941\u0906\u0924 \u0938\u0947, `datetime` \u092E\u093E\u0928\u0915 \u092A\
  \u0941\u0938\u094D\u0924\u0915\u093E\u0932\u092F \u092E\u0947\u0902 \u090F\u0915\
  \ \u092E\u0941\u0916\u094D\u092F \u0915\u094B\u0917 \u0939\u0948\u0964 \u0935\u093F\
  \u0915\u0932\u094D\u092A\u094B\u0902 \u092E\u0947\u0902 `time` \u092E\u0949\u0921\
  \u094D\u092F\u0942\u0932 \u092D\u0940 \u0939\u0948, \u092A\u0930 \u0935\u0939 \u0938\
  \u0940\u092E\u093F\u0924 \u0939\u0948\u0964 \u0924\u093E\u0930\u0940\u0916\u094B\
  \u0902 \u0915\u0940 \u0924\u0941\u0932\u0928\u093E \u0915\u0930\u0924\u0947 \u0938\
  \u092E\u092F, `datetime` \u0911\u092C\u094D\u091C\u0947\u0915\u094D\u091F\u094D\u0938\
  \ \u0915\u094B \u0938\u0940\u0927\u0947 \u0924\u0941\u0932\u0928\u093E \u0911\u092A\
  \u0930\u0947\u091F\u0930\u094B\u0902 \u0915\u0947 \u0938\u093E\u0925 compare \u0915\
  \u093F\u092F\u093E \u091C\u093E \u0938\u0915\u0924\u093E \u0939\u0948, \u091C\u0948\
  \u0938\u0947 `<`, `>`, `==`\u0964 \u092C\u0947\u0939\u0924\u0930 functionality \u0914\
  \u0930 third-party extensions \u0915\u0947 \u0932\u093F\u090F, `dateutil` \u092A\
  \u0948\u0915\u0947\u091C \u092D\u0940 \u0909\u092A\u0932\u092C\u094D\u0927 \u0939\
  \u0948\u0964."
title: "\u0926\u094B \u0924\u093E\u0930\u0940\u0916\u094B\u0902 \u0915\u0940 \u0924\
  \u0941\u0932\u0928\u093E"
weight: 27
---

## कैसे करें? (How to:)
```Python
from datetime import datetime

# तारीखें बनाएं
date1 = datetime(2023, 3, 15)
date2 = datetime(2023, 4, 25)

# तारीखों की तुलना करें
print("date1 छोटी है date2 से:", date1 < date2)
print("date1 और date2 बराबर हैं:", date1 == date2)
print("date1 बड़ी है date2 से:", date1 > date2)
```

Sample Output:
```
date1 छोटी है date2 से: True
date1 और date2 बराबर हैं: False
date1 बड़ी है date2 से: False
```

## गहराई से जानकारी (Deep Dive)
Python में दो तारीखों की तुलना `datetime` मॉड्यूल से की जा सकती है। 1990 में Python की शुरुआत से, `datetime` मानक पुस्तकालय में एक मुख्य कोग है। विकल्पों में `time` मॉड्यूल भी है, पर वह सीमित है। तारीखों की तुलना करते समय, `datetime` ऑब्जेक्ट्स को सीधे तुलना ऑपरेटरों के साथ compare किया जा सकता है, जैसे `<`, `>`, `==`। बेहतर functionality और third-party extensions के लिए, `dateutil` पैकेज भी उपलब्ध है।

## और भी (See Also)
- datetime मॉड्यूल की आधिकारिक डाॅक्स: https://docs.python.org/3/library/datetime.html
- dateutil पैकेज: https://dateutil.readthedocs.io/en/stable/
- Python की टाइम फंक्शंस पर गहराई से समझ: https://realpython.com/python-time-module/
