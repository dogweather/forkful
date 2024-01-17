---
title:                "स्ट्रिंग को लोअर केस में रूपांतरण करना"
html_title:           "Python: स्ट्रिंग को लोअर केस में रूपांतरण करना"
simple_title:         "स्ट्रिंग को लोअर केस में रूपांतरण करना"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

#क्या और क्यों?
किसी भी भाषा में वर्टिकल केस के बजाय स्ट्रिंग को लोअर केस में बदलने को स्ट्रिंग को लोअर केस में बदलने कहते हैं। इसका मुख्य उद्देश्य स्ट्रिंग के चरित्रों को सरल और समानान्वित बनाना है। प्रोग्रामर्स करने के लिए इसे उपयोग करते हैं ताकि उनके कोड में स्ट्रिंग कॉम्पेरिसन करना आसान हो।

## कैसे करें?
आप स्ट्रिंग के `.lower()` method का उपयोग करके बहुत आसानी से स्ट्रिंग को लोअर केस में बदल सकते हैं। नीचे दिए गए उदाहरण कोड ब्लॉक में देखें:

```Python
string = "HELLO WORLD"
print(string.lower())
# output: hello world 
```

## गहरी खोज
- हिस्टोरिकल कॉन्टेक्स्ट: पहले, कंप्यूटर आधारित प्रोग्राम में स्ट्रिंग का केस बहुत महत्वपूर्ण था। लेकिन आजकल, यह अहामियत धीरे-धीरे खत्म होती जा रही है।
- विकल्प: आप स्ट्रिंग को `str.casefold()` method का भी उपयोग कर सकते हैं। इसमें और भी अनेक लैंग्वेज में वर्टिकल व स्मॉल केस को समान रूप से खेलने के लिए अनेक ऑप्शंस होते हैं।
- अंतर्निहित जानकारी: स्ट्रिंग को लोअर केस में बदलने के लिए `lower()` method एक inplace operation है। इसका मतलब है कि `string` variable का वैल्यू बदल जाता है। इसलिए, एक नया स्ट्रिंग वेरिएबल बनाने के लिए आपको एक `new_string = string.lower()` line जोड़नी होगी। इससे बचने के लिए, आप `str.casefold()` का इस्तेमाल कर सकते हैं जो एक नया स्ट्रिंग object बनाता है।

## अन्य स्रोत
- [Python documentation on `lower()` method](https://docs.python.org/3/library/stdtypes.html#str.lower)
- [Python documentation on `casefold()` method](https://docs.python.org/3/library/stdtypes.html#str.casefold)