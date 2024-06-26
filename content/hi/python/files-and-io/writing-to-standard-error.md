---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:35:13.884833-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: Python \u0915\u093E\
  \ \u092C\u093F\u0932\u094D\u091F-\u0907\u0928 `sys` \u092E\u0949\u0921\u094D\u092F\
  \u0942\u0932 `stderr` \u092A\u0930 \u0938\u094D\u092A\u0937\u094D\u091F \u0930\u0942\
  \u092A \u0938\u0947 \u0932\u0947\u0916\u0928 \u0915\u0940 \u0905\u0928\u0941\u092E\
  \u0924\u093F \u0926\u0947\u0924\u093E \u0939\u0948\u0964 \u092F\u0939 \u0926\u0943\
  \u0937\u094D\u091F\u093F\u0915\u094B\u0923 \u0938\u093E\u0927\u093E\u0930\u0923\
  \ \u0924\u094D\u0930\u0941\u091F\u093F \u0938\u0902\u0926\u0947\u0936\u094B\u0902\
  \ \u092F\u093E \u0928\u093F\u0926\u093E\u0928\u093E\u0924\u094D\u092E\u0915\u0924\
  \u093E \u0915\u0947 \u0932\u093F\u090F\u2026"
lastmod: '2024-03-13T22:44:51.620028-06:00'
model: gpt-4-0125-preview
summary: "Python \u0915\u093E \u092C\u093F\u0932\u094D\u091F-\u0907\u0928 `sys` \u092E\
  \u0949\u0921\u094D\u092F\u0942\u0932 `stderr` \u092A\u0930 \u0938\u094D\u092A\u0937\
  \u094D\u091F \u0930\u0942\u092A \u0938\u0947 \u0932\u0947\u0916\u0928 \u0915\u0940\
  \ \u0905\u0928\u0941\u092E\u0924\u093F \u0926\u0947\u0924\u093E \u0939\u0948\u0964\
  \ \u092F\u0939 \u0926\u0943\u0937\u094D\u091F\u093F\u0915\u094B\u0923 \u0938\u093E\
  \u0927\u093E\u0930\u0923 \u0924\u094D\u0930\u0941\u091F\u093F \u0938\u0902\u0926\
  \u0947\u0936\u094B\u0902 \u092F\u093E \u0928\u093F\u0926\u093E\u0928\u093E\u0924\
  \u094D\u092E\u0915\u0924\u093E \u0915\u0947 \u0932\u093F\u090F \u0938\u0940\u0927\
  \u093E \u0939\u0948\u0964."
title: "\u092E\u093E\u0928\u0915 \u0924\u094D\u0930\u0941\u091F\u093F \u0915\u0947\
  \ \u0932\u093F\u090F \u0932\u093F\u0916\u0928\u093E"
weight: 25
---

## कैसे करें:


### `sys.stderr` का उपयोग करते हुए
Python का बिल्ट-इन `sys` मॉड्यूल `stderr` पर स्पष्ट रूप से लेखन की अनुमति देता है। यह दृष्टिकोण साधारण त्रुटि संदेशों या निदानात्मकता के लिए सीधा है।

```python
import sys

sys.stderr.write('Error: कुछ गलत हो गया.\n')
```
नमूना आउटपुट (stderr पर):
```
Error: कुछ गलत हो गया.
```

### `print` फ़ंक्शन का उपयोग करते हुए
Python का `print` फ़ंक्शन अपने आउटपुट को `stderr` पर निर्देशित कर सकता है जब `file` पैरामीटर को निर्दिष्ट किया जाता है। यह विधि त्रुटि संदेशों को संभालते समय `print` की उपयोगकर्ता-मित्रता का लाभ उठाने के लिए उपयोगी है।
```python
from sys import stderr

print('Error: मॉड्यूल में विफलता.', file=stderr)
```
नमूना आउटपुट (stderr पर):
```
Error: मॉड्यूल में विफलता.
```

### `logging` मॉड्यूल का उपयोग करते हुए
एक व्यापक समाधान के लिए, Python का `logging` मॉड्यूल संदेशों को `stderr` और बहुत कुछ पर निर्देशित कर सकता है, जैसे कि एक फ़ाइल में लिखना या संदेश प्रारूप को अनुकूलित करना। यह विधि उन अनुप्रयोगों के लिए सबसे अच्छी है जिन्हें विभिन्न स्तरों की लॉगिंग, संदेश प्रारूपण, या गंतव्यों की आवश्यकता होती है।
```python
import logging

logging.basicConfig(level=logging.WARNING)
logger = logging.getLogger(__name__)

logger.error('Error: डेटाबेस कनेक्शन विफल रहा.')
```
नमूना आउटपुट (stderr पर):
```
ERROR:__main__:Error: डेटाबेस कनेक्शन विफल रहा.
```

### तृतीय-पक्ष पुस्तकालय: `loguru`
`loguru` एक लोकप्रिय तृतीय-पक्ष पुस्तकालय है जो Python अनुप्रयोगों में लॉगिंग को सरल बनाता है। यह अन्य सुविधाओं के बीच, गलतियाँ स्वचालित रूप से `stderr` पर निर्देशित करता है।

`loguru` का उपयोग करने के लिए, पहले इसे पिप के माध्यम से इंस्टॉल करें:
```shell
pip install loguru
```

फिर, इसे अपनी पायथन स्क्रिप्ट में इस प्रकार शामिल करें:
```python
from loguru import logger

logger.error('Error: फ़ाइल खोलने में विफल.')
```
नमूना आउटपुट (stderr पर):
```
2023-04-05 12:00:00.000 | ERROR    | __main__:<module>:6 - Error: फ़ाइल खोलने में विफल.
```
