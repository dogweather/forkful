---
title:                "जटिल संख्याओं के साथ काम करना"
date:                  2024-01-26T04:41:53.094304-07:00
model:                 gpt-4-0125-preview
simple_title:         "जटिल संख्याओं के साथ काम करना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
जटिल संख्याएं एक वास्तविक भाग और एक काल्पनिक भाग (`a + bi`) से युक्त होती हैं। वे विभिन्न क्षेत्रों जैसे कि इलेक्ट्रिकल इंजीनियरिंग और क्वांटम कंप्यूटिंग में उपयोगी होती हैं। प्रोग्रामर उन्हें ऐसे समीकरणों को मॉडल करने के लिए उपयोग करते हैं जिन्हें केवल वास्तविक संख्याओं का उपयोग करके हल नहीं किया जा सकता।

## कैसे:
Gleam में जटिल संख्याओं के लिए देशी समर्थन का अभाव है। आप सामान्यतः अपना खुद का बनाते हैं या किसी पुस्तकालय को ढूंढते हैं। यहाँ एक त्वरित उदाहरण है कि कैसे आप बुनियादी कार्रवाईयों को क्रियान्वित कर सकते हैं:

```gleam
प्रकार जटिल {
  जटिल(फ्लोट, फ्लोट)
}

fn जोड़(c1: जटिल, c2: जटिल) -> जटिल {
  दें जटिल(a, b) = c1
  दें जटिल(x, y) = c2
  जटिल(a + x, b + y)
}

fn गुणा(c1: जटिल, c2: जटिल) -> जटिल {
  दें जटिल(a, b) = c1
  दें जटिल(x, y) = c2
  जटिल(a*x - b*y, a*y + b*x)
}

fn मुख्य() {
  दें num1 = जटिल(1.0, 2.0)
  दें num2 = जटिल(3.0, 4.0)
  दें sum = जोड़(num1, num2)
  दें product = गुणा(num1, num2)

  sum // जटिल(4.0, 6.0)
  product // जटिल(-5.0, 10.0)
}
```

## गहन विचार

जटिल संख्याओं को पहली बार 16वीं शताब्दी में Gerolamo Cardano द्वारा अधिक औपचारिक रूप से लिखित में दर्ज किया गया था। वे वास्तविक संख्याओं का एक स्वाभाविक विस्तार हैं। हालांकि, Gleam जैसी एक युवा भाषा में - जो प्रदर्शन और प्रकार सुरक्षा को प्राथमिकता देती है - ऐसी सुविधाएँ बुनियादी (या आप DIY) हैं।

कुछ अन्य भाषाओं में, जैसे कि Python, जटिल संख्याएँ निर्मित होती हैं (`3+4j`), जीवन को आसान बनाती हैं। Rust या Haskell में, आपके पास पुस्तकालय होते हैं जो बॉक्स से बाहर उन्नत कार्यक्षमताएं प्रदान करते हैं।

Gleam का दृष्टिकोण यह है कि आपको सभी पहलुओं को संभालना पड़ता है: अंकगणित, ध्रुवीय निर्देशांक, घातीय रूप, आदि। कार्यकुशल, सटीक कार्रवाइयाँ कार्यान्वित करना सावधानीपूर्ण प्रोग्रामिंग की मांग करता है, खासकर जब आप विचार करते हैं कि फ्लोटिंग-पॉइंट व्यवहार आपके परिणामों को कैसे प्रभावित कर सकता है।

विशेष रूप से किनारे के मामलों का परीक्षण करना न भूलें! जटिल अनंत और NaN (संख्या नहीं) मानों को संभालना, यदि आप सावधान नहीं हैं तो आपको परेशान कर सकता है।

## देखें अतिरिक्त
और अधिक अच्छाइयों के लिए, यहां आप गोताखोरी कर सकते हैं:
- [Gleam की आधिकारिक डॉक्स](https://gleam.run/documentation/)
- प्रेरणा के लिए अन्य भाषाओं की पुस्तकालयों को खोजें, जैसे कि Rust का [num-complex](https://crates.io/crates/num-complex) या Python का [cmath मॉड्यूल](https://docs.python.org/3/library/cmath.html)।