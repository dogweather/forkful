---
aliases:
- /hi/python/printing-debug-output/
date: 2024-01-20 17:53:39.168921-07:00
description: "Debug output \u0915\u094D\u092F\u093E \u0939\u0948? Simple, \u091C\u092C\
  \ \u0906\u092A \u0915\u094B\u0921\u093F\u0902\u0917 \u0915\u0930\u0924\u0947 \u0939\
  \u0948\u0902, \u0914\u0930 \u0905\u092A\u0928\u0947 \u092A\u094D\u0930\u094B\u0917\
  \u094D\u0930\u093E\u092E \u0915\u0940 \u091C\u093E\u0902\u091A \u092A\u0921\u093C\
  \u0924\u093E\u0932 \u0915\u0930\u0928\u093E \u091A\u093E\u0939\u0924\u0947 \u0939\
  \u0948\u0902, \u0924\u094B \u0906\u092A\u0915\u094B \u092A\u0924\u093E \u091A\u0932\
  \u0924\u093E \u0939\u0948 \u0915\u093F \u0915\u094C\u0928 \u0938\u093E \u0939\u093F\
  \u0938\u094D\u0938\u093E \u0915\u0948\u0938\u0947 \u091A\u0932 \u0930\u0939\u093E\
  \ \u0939\u0948,\u2026"
lastmod: 2024-02-18 23:09:02.652216
model: gpt-4-1106-preview
summary: "Debug output \u0915\u094D\u092F\u093E \u0939\u0948? Simple, \u091C\u092C\
  \ \u0906\u092A \u0915\u094B\u0921\u093F\u0902\u0917 \u0915\u0930\u0924\u0947 \u0939\
  \u0948\u0902, \u0914\u0930 \u0905\u092A\u0928\u0947 \u092A\u094D\u0930\u094B\u0917\
  \u094D\u0930\u093E\u092E \u0915\u0940 \u091C\u093E\u0902\u091A \u092A\u0921\u093C\
  \u0924\u093E\u0932 \u0915\u0930\u0928\u093E \u091A\u093E\u0939\u0924\u0947 \u0939\
  \u0948\u0902, \u0924\u094B \u0906\u092A\u0915\u094B \u092A\u0924\u093E \u091A\u0932\
  \u0924\u093E \u0939\u0948 \u0915\u093F \u0915\u094C\u0928 \u0938\u093E \u0939\u093F\
  \u0938\u094D\u0938\u093E \u0915\u0948\u0938\u0947 \u091A\u0932 \u0930\u0939\u093E\
  \ \u0939\u0948,\u2026"
title: "\u0921\u0940\u092C\u0917 \u0906\u0909\u091F\u092A\u0941\u091F \u092A\u094D\
  \u0930\u093F\u0902\u091F \u0915\u0930\u0928\u093E"
---

{{< edit_this_page >}}

## What & Why? (क्या है और क्यों?)
Debug output क्या है? Simple, जब आप कोडिंग करते हैं, और अपने प्रोग्राम की जांच पड़ताल करना चाहते हैं, तो आपको पता चलता है कि कौन सा हिस्सा कैसे चल रहा है, इसे हम debug output कहते हैं। यह महत्त्वपूर्ण क्यों है? क्योंकि यह हमें कोड में मौजूद समस्याओं की पहचान करने और उन्हें ठीक करने में मदद करता है।

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
