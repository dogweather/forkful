---
title:                "Python: एक स्ट्रिंग को निचले केस में रूपांतरण करना"
programming_language: "Python"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/python/converting-a-string-to-lower-case.md"
---

{{< edit_this_page >}}

# क्यों

एक डेटा साइंटिस्ट या पाइथन प्रोग्रामर के रूप में, आपको अपनी पाइथन कोड में दिए गए प्रॉब्लम से सामना करना पड़ता है। इसमें से एक चुनौती हो सकती है कि दो स्ट्रिंग के मूल्यों को तुलना करने के लिए आपको उन्हें निचे केस में बदलने की ज़रूरत हो सकती है। इसलिए, स्ट्रिंग को लवर केस में कन्वर्ट करना आवश्यक हो सकता है।

## कैसे करें

यदि आपको डेटा साइंस या पाइथन में थोड़ा भी अनुभव है, तो आप जानते होंगे कि स्ट्रिंग के मामले में पाइथन एक बहुत ही प्रभावी भाषा है। इसलिए, स्ट्रिंग को निचे केस में कन्वर्ट करने के लिए भी पाइथन में कई आसान तरीके हैं। यहां कुछ उदाहरण हैं:

```python
# सामान्य तरीका
string = "Hello World"
lower_string = string.lower()
print(lower_string) # output: hello world

# सामूहिक काम से करने के लिए
string = "Hello World"
lower_string = "".join([char.lower() for char in string])
print(lower_string) # output: hello world

# अल्फाबेटिकल क्रम में कन्वर्ट करने के लिए
string = "Hello World"
lower_string = "".join(sorted(string)).lower()
print(lower_string) # output: dehllloorw

```

आप ऊपर दिए गए कोड ब्लॉक में पाइथन वर्णमाला का प्रयोग करके आसानी से स्ट्रिंग को निचे केस में कन्वर्ट कर सकते हैं.

## गहराई में जाएं

स्ट्रिंग को लवर केस में कन्वर्ट करने के सभी तरीके सामान्य रूप से प्रोग्रामिंग में उपयोग किए जाते हैं। इसलिए, आपको इस विषय पर गहराई से जानने की आवश्यकता नहीं है। हालांकि, स्ट्रिंग को खंडित क