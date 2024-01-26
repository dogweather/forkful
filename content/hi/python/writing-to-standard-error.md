---
title:                "मानक त्रुटि में लिखना"
html_title:           "Arduino: मानक त्रुटि में लिखना"
simple_title:         "मानक त्रुटि में लिखना"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)
स्टैंडर्ड एरर में लिखने का मतलब है एरर मैसेजेस को एक खास आउटपुट में दिखाना। प्रोग्रामर्स इसे इसलिए करते हैं ताकि एरर्स को आसानी से पहचाना जा सके और उन्हें नॉर्मल आउटपुट से अलग किया जा सके।

## कैसे करें? (How to:)
```Python
import sys

# सही आउटपुट के लिए
print("यह एक मैसेज है।")

# एरर मैसेज के लिए
print("यह एक एरर मैसेज है।", file=sys.stderr)
```

सैंपल आउटपुट:
```
यह एक मैसेज है।
यह एक एरर मैसेज है।
```

यहाँ पर, पहला मैसेज स्टैंडर्ड आउटपुट (stdout) में जाएगा और दूसरा स्टैंडर्ड एरर (stderr) में।

## गहराई में जानकारी (Deep Dive)
हिस्टोरिकल कॉन्टेक्स्ट: UNIX सिस्टम्स से शुरू हुए, स्टैंडर्ड एरर का मुख्य काम था एरर मैसेजेस को स्टैंडर्ड आउटपुट से अलग रखना।

विकल्प: लॉगिंग मॉड्यूल इस्तेमाल करके भी एरर लॉग्स की मैनेजमेंट की जा सकती है।

इम्प्लिमेंटेशन डिटेल्स: `sys.stderr` एक फाइल-लाइक ऑब्जेक्ट है जो एरर्स को terminal या कमांड लाइन पर प्रिंट करता है। 

## और भी जानें (See Also)
- Python `sys` मॉड्यूल: https://docs.python.org/3/library/sys.html
- Python लॉगिंग मॉड्यूल: https://docs.python.org/3/library/logging.html
- UNIX फिलोसफी: https://en.wikipedia.org/wiki/Unix_philosophy
