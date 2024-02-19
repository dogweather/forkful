---
aliases:
- /hi/python/reading-command-line-arguments/
date: 2024-01-20 17:57:08.135486-07:00
description: "\u0915\u092E\u093E\u0902\u0921 \u0932\u093E\u0907\u0928 \u0906\u0930\
  \u094D\u0917\u094D\u092F\u0942\u092E\u0947\u0902\u091F\u094D\u0938 \u0915\u093E\
  \ \u092A\u0922\u093C\u0928\u093E, \u092F\u0939 \u0939\u094B\u0924\u093E \u0939\u0948\
  \ \u0915\u093F \u091C\u092C \u0906\u092A \u0905\u092A\u0928\u0947 \u092A\u094D\u0930\
  \u094B\u0917\u094D\u0930\u093E\u092E \u0915\u094B \u0930\u0928 \u0915\u0930\u0924\
  \u0947 \u0939\u0948\u0902, \u0924\u094B \u092F\u0942\u091C\u0930 \u0938\u0947 \u0907\
  \u0928\u092A\u0941\u091F \u0915\u0948\u0938\u0947 \u0932\u0947\u0902\u0964 \u0907\
  \u0938\u0938\u0947 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\
  \u0938 \u0915\u094B \u092B\u094D\u0932\u0947\u0915\u094D\u0938\u093F\u092C\u093F\
  \u0932\u093F\u091F\u0940 \u092E\u093F\u0932\u0924\u0940\u2026"
lastmod: 2024-02-18 23:09:02.676615
model: gpt-4-1106-preview
summary: "\u0915\u092E\u093E\u0902\u0921 \u0932\u093E\u0907\u0928 \u0906\u0930\u094D\
  \u0917\u094D\u092F\u0942\u092E\u0947\u0902\u091F\u094D\u0938 \u0915\u093E \u092A\
  \u0922\u093C\u0928\u093E, \u092F\u0939 \u0939\u094B\u0924\u093E \u0939\u0948 \u0915\
  \u093F \u091C\u092C \u0906\u092A \u0905\u092A\u0928\u0947 \u092A\u094D\u0930\u094B\
  \u0917\u094D\u0930\u093E\u092E \u0915\u094B \u0930\u0928 \u0915\u0930\u0924\u0947\
  \ \u0939\u0948\u0902, \u0924\u094B \u092F\u0942\u091C\u0930 \u0938\u0947 \u0907\u0928\
  \u092A\u0941\u091F \u0915\u0948\u0938\u0947 \u0932\u0947\u0902\u0964 \u0907\u0938\
  \u0938\u0947 \u092A\u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938\
  \ \u0915\u094B \u092B\u094D\u0932\u0947\u0915\u094D\u0938\u093F\u092C\u093F\u0932\
  \u093F\u091F\u0940 \u092E\u093F\u0932\u0924\u0940\u2026"
title: "\u0915\u092E\u093E\u0902\u0921 \u0932\u093E\u0907\u0928 \u0906\u0930\u094D\
  \u0917\u0941\u092E\u0947\u0902\u091F\u094D\u0938 \u092A\u0922\u093C\u0928\u093E"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
कमांड लाइन आर्ग्यूमेंट्स का पढ़ना, यह होता है कि जब आप अपने प्रोग्राम को रन करते हैं, तो यूजर से इनपुट कैसे लें। इससे प्रोग्रामर्स को फ्लेक्सिबिलिटी मिलती है कि वे अपने कोड को अलग-अलग सिचुएशन के हिसाब से एडजस्ट कर सकें।

## How to: (कैसे करें?)
```python
import sys

# अर्ग्यूमेंट्स को प्रिंट करें
if len(sys.argv) > 1:
    print(f"Arguments received: {sys.argv[1:]}")
else:
    print("No arguments received.")

# Example usage: python script.py arg1 arg2 arg3
```
Sample Output: `Arguments received: ['arg1', 'arg2', 'arg3']`

## Deep Dive (गहराई में जानकारी)
कमांड लाइन आर्ग्यूमेंट्स का चलन पुराना है; यह यूनिक्स सिस्टम्स से शुरू हुआ था। `sys` मॉड्यूल का उपयोग करते हुए `argv` वेरिएबल में आर्ग्यूमेंट्स को पढ़ा जाता है। वीकल्पिक तरीके के रूप में `argparse` मॉड्यूल का इस्तेमाल होता है, जो ज्यादा उन्नत विकल्पों को संभाल सकता है।

## See Also (और देखें)
- [Python `argparse` Documentation](https://docs.python.org/3/library/argparse.html)
- [Python `sys` module](https://docs.python.org/3/library/sys.html) 
- ["Automate the Boring Stuff with Python" by Al Sweigart](https://automatetheboringstuff.com/)
