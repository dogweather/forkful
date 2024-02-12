---
title:                "कमांड लाइन आर्गुमेंट्स पढ़ना"
aliases: - /hi/python/reading-command-line-arguments.md
date:                  2024-01-20T17:57:08.135486-07:00
model:                 gpt-4-1106-preview
simple_title:         "कमांड लाइन आर्गुमेंट्स पढ़ना"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/reading-command-line-arguments.md"
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
