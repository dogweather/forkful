---
date: 2024-01-20 17:55:57.183071-07:00
description: "\u091F\u0947\u0915\u094D\u0938\u094D\u091F \u092B\u093C\u093E\u0907\u0932\
  \ \u092A\u0922\u093C\u0928\u093E \u092E\u0924\u0932\u092C \u0921\u0947\u091F\u093E\
  \ \u0915\u094B \u092B\u093C\u093E\u0907\u0932 \u0938\u0947 \u090F\u0915\u094D\u0938\
  \u091F\u094D\u0930\u0948\u0915\u094D\u091F \u0915\u0930\u0928\u093E\u0964 \u092A\
  \u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0907\u0938\u0915\
  \u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0949\u0928\u094D\u092B\u093C\u093F\
  \u0917\u094D\u0938, \u0921\u0947\u091F\u093E \u0938\u094D\u091F\u094B\u0930\u0947\
  \u091C \u0914\u0930 \u0932\u0949\u0917\u094D\u0938 \u0915\u0947 \u0932\u093F\u090F\
  \ \u0915\u0930\u0924\u0947 \u0939\u0948\u0902\u0964"
lastmod: '2024-03-13T22:44:51.621665-06:00'
model: gpt-4-1106-preview
summary: "\u091F\u0947\u0915\u094D\u0938\u094D\u091F \u092B\u093C\u093E\u0907\u0932\
  \ \u092A\u0922\u093C\u0928\u093E \u092E\u0924\u0932\u092C \u0921\u0947\u091F\u093E\
  \ \u0915\u094B \u092B\u093C\u093E\u0907\u0932 \u0938\u0947 \u090F\u0915\u094D\u0938\
  \u091F\u094D\u0930\u0948\u0915\u094D\u091F \u0915\u0930\u0928\u093E\u0964 \u092A\
  \u094D\u0930\u094B\u0917\u094D\u0930\u093E\u092E\u0930\u094D\u0938 \u0907\u0938\u0915\
  \u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0949\u0928\u094D\u092B\u093C\u093F\
  \u0917\u094D\u0938, \u0921\u0947\u091F\u093E \u0938\u094D\u091F\u094B\u0930\u0947\
  \u091C \u0914\u0930 \u0932\u0949\u0917\u094D\u0938 \u0915\u0947 \u0932\u093F\u090F\
  \ \u0915\u0930\u0924\u0947 \u0939\u0948\u0902\u0964."
title: "\u091F\u0947\u0915\u094D\u0938\u094D\u091F \u092B\u093C\u093E\u0907\u0932\
  \ \u092A\u0922\u093C\u0928\u093E"
weight: 22
---

## What & Why? (क्या और क्यों?)
टेक्स्ट फ़ाइल पढ़ना मतलब डेटा को फ़ाइल से एक्सट्रैक्ट करना। प्रोग्रामर्स इसका उपयोग कॉन्फ़िग्स, डेटा स्टोरेज और लॉग्स के लिए करते हैं।

## How to: (कैसे करें:)
```Python
# टेक्स्ट फ़ाइल खोलना और पढ़ना
with open('example.txt', 'r') as file:
    content = file.read()
    print(content)

# सैम्पल आउटपुट
# Hello, this is a text file.
# There are many like it, but this one is mine.
```

```Python
# लाइन बाय लाइन पढ़ना
with open('example.txt', 'r') as file:
    for line in file:
        print(line.strip())

# सैम्पल आउटपुट
# Hello, this is a text file.
# There are many like it, but this one is mine.
```

## Deep Dive (गहराई में जानकारी):
प्राचीन समय से ही टेक्स्ट फाइलें डेटा स्टोर करने का एक सरल तरीका रही हैं। वे आम तौर पर यूनिकोड या एएससीआईआई एनकोडिंग में होती हैं। वर्तमान में जेसन, एक्सएमएल जैसे फॉर्मेट्स का उपयोग होता है, मगर सादगी के लिए अभी भी टेक्स्ट फाइलें प्रचलित हैं। `read()`, `readline()`, `readlines()` जैसे फंक्शन्स का उपयोग करके साधारणतया फाइल की सारी लाइन्स या स्पेसिफिक लाइन्स को पढ़ा जा सकता है। `with open()` का यूज करने से फाइल ऑटोमैटिकली क्लोज हो जाती है जिससे मेमोरी लीकेज की प्रॉब्लम नहीं होती।

## See Also (अधिक जानकारी के लिए):
- [Python File I/O Documentation](https://docs.python.org/3/tutorial/inputoutput.html#reading-and-writing-files) - पाइथन डॉक्स में फाइल I/O पर और जानकारी मिलेगी।
- [RealPython Tutorial on File I/O](https://realpython.com/read-write-files-python/) - रियल पाइथन पर फाइल I/O का विस्तार से ट्यूटोरियल।
- [GeeksforGeeks Python File Handling Guide](https://www.geeksforgeeks.org/file-handling-python/) - जीक्सफॉरजीक्स पर पाइथन फाइल हैंडलिंग गाइड।
