---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:57.592698-07:00
description: "\u0915\u0948\u0938\u0947: \u092A\u093E\u092F\u0925\u0928 `os` \u0914\
  \u0930 `pathlib` \u092E\u0949\u0921\u094D\u092F\u0942\u0932 \u0915\u093E \u0909\u092A\
  \u092F\u094B\u0917 \u0915\u0930\u0915\u0947 \u0921\u093E\u092F\u0930\u0947\u0915\
  \u094D\u091F\u0930\u0940 \u0915\u0947 \u0905\u0938\u094D\u0924\u093F\u0924\u094D\
  \u0935 \u0915\u0940 \u091C\u093E\u0902\u091A \u0915\u0930\u0928\u0947 \u0915\u0947\
  \ \u0932\u093F\u090F \u0938\u094D\u0935\u0926\u0947\u0936\u0940 \u0924\u0930\u0940\
  \u0915\u0947 \u092A\u094D\u0930\u0926\u093E\u0928 \u0915\u0930\u0924\u093E \u0939\
  \u0948\u0964 \u092F\u0939\u093E\u0901 \u0926\u094B\u0928\u094B\u0902 \u0915\u0947\
  \ \u0909\u0926\u093E\u0939\u0930\u0923 \u0926\u093F\u090F \u0917\u090F\u2026"
lastmod: '2024-03-13T22:44:51.616673-06:00'
model: gpt-4-0125-preview
summary: "\u092A\u093E\u092F\u0925\u0928 `os` \u0914\u0930 `pathlib` \u092E\u0949\u0921\
  \u094D\u092F\u0942\u0932 \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\
  \u0915\u0947 \u0921\u093E\u092F\u0930\u0947\u0915\u094D\u091F\u0930\u0940 \u0915\
  \u0947 \u0905\u0938\u094D\u0924\u093F\u0924\u094D\u0935 \u0915\u0940 \u091C\u093E\
  \u0902\u091A \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u0938\u094D\
  \u0935\u0926\u0947\u0936\u0940 \u0924\u0930\u0940\u0915\u0947 \u092A\u094D\u0930\
  \u0926\u093E\u0928 \u0915\u0930\u0924\u093E \u0939\u0948\u0964 \u092F\u0939\u093E\
  \u0901 \u0926\u094B\u0928\u094B\u0902 \u0915\u0947 \u0909\u0926\u093E\u0939\u0930\
  \u0923 \u0926\u093F\u090F \u0917\u090F \u0939\u0948\u0902."
title: "\u0921\u093E\u092F\u0930\u0947\u0915\u094D\u091F\u0930\u0940 \u092E\u094C\u091C\
  \u0942\u0926 \u0939\u0948 \u092F\u093E \u0928\u0939\u0940\u0902 \u091C\u093E\u0901\
  \u091A\u0928\u093E"
weight: 20
---

## कैसे:
पायथन `os` और `pathlib` मॉड्यूल का उपयोग करके डायरेक्टरी के अस्तित्व की जांच करने के लिए स्वदेशी तरीके प्रदान करता है। यहाँ दोनों के उदाहरण दिए गए हैं:

### `os` मॉड्यूल का उपयोग करके
```python
import os

# डायरेक्टरी पथ निर्दिष्ट करें
dir_path = "/path/to/directory"

# जांचें कि डायरेक्टरी मौजूद है या नहीं
if os.path.isdir(dir_path):
    print(f"डायरेक्टरी {dir_path} मौजूद है।")
else:
    print(f"डायरेक्टरी {dir_path} मौजूद नहीं है।")
```

### `pathlib` मॉड्यूल का उपयोग करके
```python
from pathlib import Path

# डायरेक्टरी पथ निर्दिष्ट करें
dir_path = Path("/path/to/directory")

# जांचें कि डायरेक्टरी मौजूद है या नहीं
if dir_path.is_dir():
    print(f"डायरेक्टरी {dir_path} मौजूद है।")
else:
    print(f"डायरेक्टरी {dir_path} मौजूद नहीं है।")
```

### तृतीय-पक्ष पुस्तकालय
हालांकि पायथन की स्टैंडर्ड लाइब्रेरी यह जांचने के लिए पर्याप्त है कि कोई डायरेक्टरी मौजूद है या नहीं, `pathlib2` जैसी पुस्तकालयें पायथन संस्करणों में संगतिपूर्णता या अतिरिक्त कार्यक्षमता के लिए विकल्प हो सकती हैं।

***नोट:*** नवीनतम पायथन संस्करणों के अनुसार, `pathlib` अधिकांश उपयोग के मामलों के लिए पर्याप्त रोबस्ट है, जिससे इस विशेष कार्य के लिए तृतीय-पक्ष पुस्तकालयों की आवश्यकता कम हो जाती है।
