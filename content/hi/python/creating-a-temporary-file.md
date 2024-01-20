---
title:                "एक अस्थायी फ़ाइल बनाना"
html_title:           "Arduino: एक अस्थायी फ़ाइल बनाना"
simple_title:         "एक अस्थायी फ़ाइल बनाना"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

अस्थायी फ़ाइल बनाना मतलब है एक फ़ाइल को तात्कालिक या अस्थायी रूप से बनाना, जो की प्रोग्राम चलने के बाद खुद ही हट जाती है। क्रिएटिंग अस्थायी फ़ाइल्स की मदद से प्रोग्रामर्स डिस्क स्थान को सही तरीके से प्रबंधित कर सकते हैं इसके अलावा ये तात्कालिक डाटा स्टोर करने के लिए उपयोगी होती हैं। 

## कैसे करें:

Python में, `tempfile` मॉड्यूल का उपयोग करके अस्थायी फ़ाइलें बनाई जा सकती हैं।

```Python
import tempfile

# एक अस्थायी फ़ाइल बनाएं
temp = tempfile.TemporaryFile()

# कुछ लिखें
temp.write(b'Python का इस्तेमाल करके अस्थायी फ़ाइल बनाना')

# फ़ाइल में वापस जाएं और पढ़ें
temp.seek(0)
print(temp.read())

# फ़ाइल को बंद करें, यह अब हट जाती है
temp.close()
```

## गहरा डाइव:

1. ऐतिहासिक संदर्भ: अस्थायी फ़ाइलों का उपयोग कंप्यूटर प्रोग्रामिंग के शुरुआती दिनों से ही किया जा रहा है, लेकिन Python में `tempfile` मॉड्यूल का परिचय Python 2.3 में हुआ था।
2. विकल्प: अस्थायी डाटा को स्टोर करने के लिए `StringIO` या `BytesIO` जैसे इन-मेमोरी संगठनों का भी उपयोग कर सकते हैं, लेकिन `tempfile` विशेष मेमोरी नहीं लेता।
3. कार्यान्वयन विवरण: `tempfile` तात्कालिक फ़ाइलों को सिस्टम की अस्थायी फ़ाइल डायरेक्टरी में बनाता है, जो कि सिस्टम से सिस्टम अलग होती है।

## देखें भी:

1. [Python `tempfile` डॉक्यूमेंटेशन](https://docs.python.org/3/library/tempfile.html)
2. [Python Memory Management](https://realpython.com/python-memory-management/)