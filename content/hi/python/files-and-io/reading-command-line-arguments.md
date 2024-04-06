---
date: 2024-01-20 17:57:08.135486-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902?) Sample\
  \ Output: `Arguments received: ['arg1', 'arg2', 'arg3']`."
lastmod: '2024-04-05T21:53:53.637142-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902?) Sample Output."
title: "\u0915\u092E\u093E\u0902\u0921 \u0932\u093E\u0907\u0928 \u0906\u0930\u094D\
  \u0917\u0941\u092E\u0947\u0902\u091F\u094D\u0938 \u092A\u0922\u093C\u0928\u093E"
weight: 23
---

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
