---
title:                "डायरेक्टरी मौजूद है या नहीं जाँचना"
date:                  2024-02-03T19:08:57.592698-07:00
model:                 gpt-4-0125-preview
simple_title:         "डायरेक्टरी मौजूद है या नहीं जाँचना"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/checking-if-a-directory-exists.md"
changelog:
  - 2024-02-03, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## क्या और क्यों?
पायथन में यह जांचना कि कोई डायरेक्टरी मौजूद है या नहीं, फाइल सिस्टम में किसी फोल्डर की उपस्थिति की पुष्टि करने के बारे में है इससे पहले कि फाइलें पढ़ने या लिखने जैसे ऑपरेशन किए जाएँ। प्रोग्रामर ऐसा करते हैं ताकि `FileNotFoundError` जैसी त्रुटियों से बच सकें, सुनिश्चित करते हैं कि एप्लिकेशन विश्वसनीय ढंग से व्यवहार करे और डायरेक्टरीज के साथ इंटरैक्ट करने की कोशिश करते समय क्रैश न हो।

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