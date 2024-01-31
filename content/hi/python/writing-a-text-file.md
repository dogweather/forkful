---
title:                "टेक्स्ट फाइल लिखना"
date:                  2024-01-19
simple_title:         "टेक्स्ट फाइल लिखना"

tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/writing-a-text-file.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
पाठ फाइल लिखना यह है कि डाटा को टेक्स्ट (सादा पाठ) रूप में फाइल में सहेजना। प्रोग्रामर इसे डाटा को संग्रहित करने, कॉन्फ़िगुरेशन सेटिंग्स संचय करने या लॉग फाइल बनाने के लिए करते हैं।

## How to: (कैसे करें:)
```Python
# एक साधारण टेक्स्ट फाइल बनाना और लिखना
text_data = "नमस्ते पायथन प्रोग्रामिंग!"
with open('example.txt', 'w', encoding='utf-8') as file:
    file.write(text_data)

# फाइल को फिर से पढ़ कर चेक करें
with open('example.txt', 'r', encoding='utf-8') as file:
    print(file.read())

# आउटपुट: नमस्ते पायथन प्रोग्रामिंग!
```

## Deep Dive (गहराई से जानकारी)
प्राचीन समय से, डाटा को सहेजना महत्वपूर्ण रहा है; पहले कागज पर अब डिजिटल रूप में। पायथन में टेक्स्ट फाइल लिखने के विकल्पों में `open()` फंक्शन इस्तेमाल होता है जो विभिन्न मोड्स ('w' लिखने के लिए, 'r' पढ़ने के लिए इत्यादि) में फाइल को खोलता है। UTF-8 एनकोडिंग महत्वपूर्ण है जब हम अंतर्राष्ट्रीय करैक्टर्स का उपयोग करते हैं।

## See Also (और जानकारी के लिए)
- पायथन का निर्देशिका `io` (https://docs.python.org/3/library/io.html)
- पायथन `open()` फंक्शन (https://docs.python.org/3/library/functions.html#open)
- UTF-8 एनकोडिंग (https://docs.python.org/3/howto/unicode.html)
