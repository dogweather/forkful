---
title:                "एक पाठ फ़ाइल पढ़ना"
html_title:           "Bash: एक पाठ फ़ाइल पढ़ना"
simple_title:         "एक पाठ फ़ाइल पढ़ना"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
तो पहली बात, एक टेक्स्ट फ़ाइल को पढ़ना क्या होता है? फ़ाइल रीडिंग का मतलब है एक टेक्स्ट फ़ाइल में संग्रहीत data को पढ़ना। प्रोग्रामर्स इसे क्यों करते हैं? डाटा का विश्लेषण करने और प्रोसेसिंग करने के लिए।

## कैसे करें:
```Python
# फ़ाइल खोलने के लिए "open" function का उपयोग करें
file = open('my_file.txt', 'r')

# फ़ाइल को पढ़ें
data = file.read()

# फ़ाइल को बंद करें
file.close()

# डेटा को प्रिंट करें
print(data)
```
उपरोक्त कोड उदाहरण का आउटपुट निम्नानुसार होगा:

```Python
'Hello World!'
```

## गहराई में:
1. **ऐतिहासिक संदर्भ:** 'open' function का इस्तेमाल करने की प्रक्रिया बहुत ही पुरानी है और इसका इस्तेमाल Python के सभी previous versions में होता आ रहा है।
2. **वैकल्पिक:** 'with' कीवर्ड का भी उपयोग किया जा सकता है जो फ़ाइल को ऑटोमेटिकली close कर देता है।
3. **Implementation Details:** 'open' function शुरू में फ़ाइल को ओपन करता है, 'read' function उसे पढ़ता है और फ़ाइल को बंद करने के लिए 'close' function का उपयोग किया जाता है।

## और देखें:
1. [Python Official Documentation](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files)
2. [File handling in Python](https://www.simplylearnt.com/topic/File-handling-in-Python)
3. [Reading and Writing to Text Files](https://www.learnpython.dev/02-introduction-to-python/110-filehandling/10-reading-and-writing-to-text-files/)
4. [Working with File I/O](https://www.geeksforgeeks.org/file-handling-python/)