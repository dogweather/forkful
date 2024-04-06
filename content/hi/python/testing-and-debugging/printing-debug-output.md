---
date: 2024-01-20 17:53:39.168921-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Debugging\
  \ \u0915\u0940 \u0939\u093F\u0938\u094D\u091F\u094D\u0930\u0940 \u092A\u0941\u0930\
  \u093E\u0928\u0940 \u0939\u0948 - \u091C\u092C \u0915\u0902\u092A\u094D\u092F\u0942\
  \u091F\u0930 \u0915\u0947 \u092E\u0947\u092E\u094B\u0930\u0940 \u092E\u0947\u0902\
  \ \u0915\u0940\u0921\u093C\u0947 (bugs) \u092B\u0902\u0938 \u091C\u093E\u092F\u093E\
  \ \u0915\u0930\u0924\u0947 \u0925\u0947\u0964 \u0906\u091C \u0915\u0932, `print`\
  \ \u0938\u094D\u091F\u0947\u091F\u092E\u0947\u0902\u091F\u094D\u0938 \u0915\u0947\
  \ \u0907\u0932\u093E\u0935\u093E, \u0939\u092E\u2026"
lastmod: '2024-04-05T22:51:06.287945-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) Debugging \u0915\u0940\
  \ \u0939\u093F\u0938\u094D\u091F\u094D\u0930\u0940 \u092A\u0941\u0930\u093E\u0928\
  \u0940 \u0939\u0948 - \u091C\u092C \u0915\u0902\u092A\u094D\u092F\u0942\u091F\u0930\
  \ \u0915\u0947 \u092E\u0947\u092E\u094B\u0930\u0940 \u092E\u0947\u0902 \u0915\u0940\
  \u0921\u093C\u0947 (bugs) \u092B\u0902\u0938 \u091C\u093E\u092F\u093E \u0915\u0930\
  \u0924\u0947 \u0925\u0947\u0964 \u0906\u091C \u0915\u0932, `print` \u0938\u094D\u091F\
  \u0947\u091F\u092E\u0947\u0902\u091F\u094D\u0938 \u0915\u0947 \u0907\u0932\u093E\
  \u0935\u093E, \u0939\u092E \u0932\u0949\u0917\u093F\u0902\u0917 \u091C\u0948\u0938\
  \u0940 \u0924\u0915\u0928\u0940\u0915 \u0915\u093E \u0907\u0938\u094D\u0924\u0947\
  \u092E\u093E\u0932 \u0915\u0930\u0924\u0947 \u0939\u0948\u0902, \u091C\u094B \u0915\
  \u0902\u091F\u094D\u0930\u094B\u0932 \u0915\u0947 \u0938\u093E\u0925 \u091C\u094D\
  \u092F\u093E\u0926\u093E detailed information \u0926\u0947\u0924\u0940 \u0939\u0948\
  \u0964 Python \u092E\u0947\u0902 `logging` \u092E\u0949\u0921\u094D\u092F\u0942\u0932\
  \ \u0915\u093E \u0907\u0938\u094D\u0924\u0947\u092E\u093E\u0932 \u090F\u0915 \u0914\
  \u0930 \u0935\u093F\u0915\u0932\u094D\u092A \u0939\u0948 \u091C\u093F\u0938\u0938\
  \u0947 \u0915\u093F \u0906\u092A \u0938\u094D\u0924\u0930 (level) \u0938\u0947\u091F\
  \ \u0915\u0930 \u0938\u0915\u0924\u0947 \u0939\u0948\u0902."
title: "\u0921\u0940\u092C\u0917 \u0906\u0909\u091F\u092A\u0941\u091F \u092A\u094D\
  \u0930\u093F\u0902\u091F \u0915\u0930\u0928\u093E"
weight: 33
---

## How to: (कैसे करें:)
```Python
# साधारण प्रिंट स्टेटमेंट
print("Hello, World!")

# किसी वेरिएबल का मान देखने के लिए
number = 50
print("The number is:", number)

# लूप के दौरान कीड़े का पता लगाने के लिए
for i in range(5):
    print(f"Loop iteration {i}")

# कोड ब्लॉक्स के अंदर और बाहर प्रिंटिंग
if number % 2 == 0:
    print(f"{number} is even.")
else:
    print(f"{number} is odd.")
```
Sample Output:
```
Hello, World!
The number is: 50
Loop iteration 0
Loop iteration 1
Loop iteration 2
Loop iteration 3
Loop iteration 4
50 is even.
```

## Deep Dive (गहराई से समझ):
Debugging की हिस्ट्री पुरानी है - जब कंप्यूटर के मेमोरी में कीड़े (bugs) फंस जाया करते थे। आज कल, `print` स्टेटमेंट्स के इलावा, हम लॉगिंग जैसी तकनीक का इस्तेमाल करते हैं, जो कंट्रोल के साथ ज्यादा detailed information देती है। Python में `logging` मॉड्यूल का इस्तेमाल एक और विकल्प है जिससे कि आप स्तर (level) सेट कर सकते हैं: DEBUG, INFO, WARNING, ERROR, और CRITICAL। यह तय करता है की कौन सी जानकारी रिकॉर्ड की जाए और कौन सी नहीं।

## See Also (और भी देखें):
- [Python Logging Module](https://docs.python.org/3/library/logging.html)
- [Python `print` function](https://docs.python.org/3/library/functions.html#print)
- [Python Debugging with PDB](https://docs.python.org/3/library/pdb.html)
