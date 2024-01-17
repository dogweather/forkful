---
title:                "एक पाठ फ़ाइल को पढ़ना"
html_title:           "Python: एक पाठ फ़ाइल को पढ़ना"
simple_title:         "एक पाठ फ़ाइल को पढ़ना"
programming_language: "Python"
category:             "Python"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/reading-a-text-file.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
एक टेक्स्ट फ़ाइल को पढ़ना क्या है और क्यों प्रोग्रामर्स इसे करते हैं, इसका सामान्य रूप से जवाब है कि संख्याओं, वर्णों, या सेहत को सुरक्षित रखने या सेव करने की जरुरत होती है।
## कैसे करें:
```python
# ‘file.txt’ फाइल को खोलें
file = open(‘file.txt’, ‘r’)

# फाइल से प्रथम पंक्ति पर पुनःटिप्पणित करें
print(file.readline())

# फाइल को बंद करें
file.close()

# फाइल से सारी पंक्तियाँ प्रिंट करें
with open(‘file.txt’, ‘r’) as file:
  for line in file:
    print(line)
```
आउटपुट:
```
प्रथम पंक्ति
दूजी पंक्ति
तीसरी पंक्ति
```
## गहराई में जाओ:
इस कार्य में इस्तेमाल किए गए प्रोग्रामिंग भाषाएं कई सालों से उपयोग में हैं। अलग-अलग भाषाओं में भी कई विकल्प उपलब्ध हैं जैसे कि ‘with’ का उपयोग करना या ‘readline()’ इस्तेमाल करना। कोनकोनकोभार दायरों को खोलने और बंद करने के लिए ‘open()’ कमांड को जरूरत के हिसाब से आगे-पीछे किया जाता है।
## इस से जुड़े भी देखें:
[अधिक जानकारी के लिए: “पायथन में फाइल को पढ़ना और लिखना”](https://www.programiz.com/python-programming/file-operation)