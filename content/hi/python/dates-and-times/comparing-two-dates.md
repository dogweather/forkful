---
aliases:
- /hi/python/comparing-two-dates/
date: 2024-01-20 17:34:05.528925-07:00
description: "Date comparison \u0915\u093E \u092E\u0924\u0932\u092C \u0939\u0948 \u0926\
  \u094B \u0924\u093E\u0930\u0940\u0916\u094B\u0902 \u0915\u0940 \u0924\u0941\u0932\
  \u0928\u093E \u0915\u0930\u0928\u093E - \u092F\u0947 \u091C\u093E\u0928\u0928\u0947\
  \ \u0915\u0947 \u0932\u093F\u090F \u0915\u093F \u0915\u094C\u0928 \u0938\u0940 \u0924\
  \u093E\u0930\u0940\u0916 \u092A\u0939\u0932\u0947 \u0939\u0948 \u092F\u093E \u092C\
  \u093E\u0926 \u092E\u0947\u0902, \u092F\u093E \u0915\u094D\u092F\u093E \u0935\u0947\
  \ \u090F\u0915 \u0939\u0940 \u0939\u0948\u0902\u0964 Programmers \u092F\u0947 \u0915\
  \u0930\u0924\u0947 \u0939\u0948\u0902\u2026"
lastmod: 2024-02-18 23:09:02.671049
model: gpt-4-1106-preview
summary: "Date comparison \u0915\u093E \u092E\u0924\u0932\u092C \u0939\u0948 \u0926\
  \u094B \u0924\u093E\u0930\u0940\u0916\u094B\u0902 \u0915\u0940 \u0924\u0941\u0932\
  \u0928\u093E \u0915\u0930\u0928\u093E - \u092F\u0947 \u091C\u093E\u0928\u0928\u0947\
  \ \u0915\u0947 \u0932\u093F\u090F \u0915\u093F \u0915\u094C\u0928 \u0938\u0940 \u0924\
  \u093E\u0930\u0940\u0916 \u092A\u0939\u0932\u0947 \u0939\u0948 \u092F\u093E \u092C\
  \u093E\u0926 \u092E\u0947\u0902, \u092F\u093E \u0915\u094D\u092F\u093E \u0935\u0947\
  \ \u090F\u0915 \u0939\u0940 \u0939\u0948\u0902\u0964 Programmers \u092F\u0947 \u0915\
  \u0930\u0924\u0947 \u0939\u0948\u0902\u2026"
title: "\u0926\u094B \u0924\u093E\u0930\u0940\u0916\u094B\u0902 \u0915\u0940 \u0924\
  \u0941\u0932\u0928\u093E"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)

Date comparison का मतलब है दो तारीखों की तुलना करना - ये जानने के लिए कि कौन सी तारीख पहले है या बाद में, या क्या वे एक ही हैं। Programmers ये करते हैं ताकि events के sequence, duration और timeouts को संभाल सकें।

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
