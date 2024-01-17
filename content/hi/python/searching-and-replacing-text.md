---
title:                "टेक्स्ट को खोजना और बदलना"
html_title:           "Python: टेक्स्ट को खोजना और बदलना"
simple_title:         "टेक्स्ट को खोजना और बदलना"
programming_language: "Python"
category:             "Python"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/searching-and-replacing-text.md"
---

{{< edit_this_page >}}

## क्या और क्यूँ?

तेकस्ट सर्चिंग और रिप्लेसिंग हमारे टेक्स्ट दस्तावेजों में कोई विशेष शब्द या वाक्य को ढूंढने और बदलने की प्रक्रिया है। प्रोग्रामर्स इसे प्रोग्रामिंग में विभिन्न उदाहरणों को सुधारने और सुधार देने के लिए करते हैं।

## कैसे करें:

```Python
# उदाहरण 1: सर्च और रिप्लेस करना
text = "मैं एक अलग शब्द हूँ।"
new_text = text.replace("अलग", "नया")
print(new_text)
# Output: मैं एक नया शब्द हूँ।

# उदाहरण 2: सारे वस्तुओं को रिप्लेस करें
text = "प्यारी प्यारी बिल्लियाँ!"
new_text = text.replace("प्यारी", "खूबसूरत")
print(new_text)
# Output: खूबसूरत खूबसूरत बिल्लियाँ!
```

## गहराई में जाएँ:

सर्चिंग और रिप्लेसिंग की प्रक्रिया को पहली बार 1968 में डॉनाल्ड कन्नथ द्वारा निर्मित सेड (SED) संपादक में दिखाया गया था। आजकल, प्याथन के अलावा अन्य प्रोग्रामिंग भाषाओं में भी सर्चिंग और रिप्लेसिंग के लिए विभिन्न फंक्शन्स और तकनीक हैं। इसके अलावा, आप टेक्स्ट सर्च और रिप्लेस करने के लिए ऑनलाइन टूल्स भी उपयोग कर सकते हैं।

## इससे जुड़े स्रोत:

- [अलग-पलग शब्दों के लिए सर्चिंग और रिप्लेसिंग करें - Real Python आरंभिक श्रेणी](https://realpython.com/search-and-replace/)
- [पाइथन में स्ट्रिंग ऑपरेशन](https://www.thepythoncode.com/article/string-methods-in-python)
- [सेड का इतिहास](https://www.gnu.org/software/sed/manual/html_node/A-Brief-History-of-sed.html)