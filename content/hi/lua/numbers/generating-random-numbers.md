---
date: 2024-01-27 20:35:53.455933-07:00
description: "\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902: Lua \u092E\u0947\u0902\
  \ `math.random` \u092B\u093C\u0902\u0915\u094D\u0936\u0928 \u0915\u0947 \u092E\u093E\
  \u0927\u094D\u092F\u092E \u0938\u0947 \u0905\u0928\u093F\u0936\u094D\u091A\u093F\
  \u0924 \u0938\u0902\u0916\u094D\u092F\u093E\u090F\u0901 \u0909\u0924\u094D\u092A\
  \u0928\u094D\u0928 \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u0928\
  \u093F\u0930\u094D\u092E\u093F\u0924 \u0938\u092E\u0930\u094D\u0925\u0928 \u092A\
  \u094D\u0930\u0926\u093E\u0928 \u0915\u093F\u092F\u093E \u0917\u092F\u093E \u0939\
  \u0948\u0964 \u0907\u0938 \u092B\u093C\u0902\u0915\u094D\u0936\u0928 \u0915\u093E\
  \ \u0909\u092A\u092F\u094B\u0917 \u0935\u093E\u0902\u091B\u093F\u0924\u2026"
lastmod: '2024-03-13T22:44:52.542419-06:00'
model: gpt-4-0125-preview
summary: "Lua \u092E\u0947\u0902 `math.random` \u092B\u093C\u0902\u0915\u094D\u0936\
  \u0928 \u0915\u0947 \u092E\u093E\u0927\u094D\u092F\u092E \u0938\u0947 \u0905\u0928\
  \u093F\u0936\u094D\u091A\u093F\u0924 \u0938\u0902\u0916\u094D\u092F\u093E\u090F\u0901\
  \ \u0909\u0924\u094D\u092A\u0928\u094D\u0928 \u0915\u0930\u0928\u0947 \u0915\u0947\
  \ \u0932\u093F\u090F \u0928\u093F\u0930\u094D\u092E\u093F\u0924 \u0938\u092E\u0930\
  \u094D\u0925\u0928 \u092A\u094D\u0930\u0926\u093E\u0928 \u0915\u093F\u092F\u093E\
  \ \u0917\u092F\u093E \u0939\u0948\u0964 \u0907\u0938 \u092B\u093C\u0902\u0915\u094D\
  \u0936\u0928 \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0935\u093E\u0902\u091B\
  \u093F\u0924 \u0909\u0924\u094D\u092A\u093E\u0926\u0928 \u0915\u0947 \u0906\u0927\
  \u093E\u0930 \u092A\u0930 \u090F\u0915\u093E\u0927\u093F\u0915 \u0924\u0930\u0940\
  \u0915\u094B\u0902 \u0938\u0947 \u0915\u093F\u092F\u093E \u091C\u093E \u0938\u0915\
  \u0924\u093E \u0939\u0948."
title: "\u092F\u093E\u0926\u0943\u091A\u094D\u091B\u093F\u0915 \u0938\u0902\u0916\u094D\
  \u092F\u093E\u090F\u0901 \u0909\u0924\u094D\u092A\u0928\u094D\u0928 \u0915\u0930\
  \u0928\u093E"
weight: 12
---

## कैसे करें:
Lua में `math.random` फ़ंक्शन के माध्यम से अनिश्चित संख्याएँ उत्पन्न करने के लिए निर्मित समर्थन प्रदान किया गया है। इस फ़ंक्शन का उपयोग वांछित उत्पादन के आधार पर एकाधिक तरीकों से किया जा सकता है:

1. **0 और 1 के बीच में एक यादृच्छिक फ्लोटिंग-पॉइंट संख्या उत्पन्न करना:**

```Lua
print(math.random())
```

नमूना उत्पादन `0.13117647051304` हो सकता है। प्रत्येक चलान से एक अलग मूल्य उत्पन्न होता है।

2. **निर्दिष्ट रेंज के भीतर एक यादृच्छिक पूर्णांक उत्पन्न करना:**

दो सीमाओं के बीच में एक यादृच्छिक पूर्णांक उत्पादित करने के लिए, आपको पहले विविधता के लिए `math.randomseed(os.time())` का उपयोग करके बीज सेट करने की आवश्यकता होती है, तब `math.random` को दो तर्कों के साथ कॉल करें:

```Lua
math.randomseed(os.time())
print(math.random(1, 10)) -- 1 और 10 के बीच में एक यादृच्छिक पूर्णांक उत्पन्न करता है
```

नमूना उत्पादन `7` हो सकता है। फिर से, उत्पादन प्रत्येक निष्पादन के साथ विविध होगा।

बीज लगाना `math.randomseed` के साथ महत्वपूर्ण होता है क्योंकि बिना इसके, `math.random` हर बार एक प्रोग्राम चलाने पर समान संख्या की श्रृंखला

## गहराई में जानकारी
Lua (और अधिकांश प्रोग्रामिंग भाषाओं) में अनिश्चित संख्याओं के उत्पादन के पीछे की तंत्र वास्तविक रूप से अनिश्चित नहीं है बल्कि प्रूडोरैंडम है, जो कि एक एल्गोरिदम द्वारा उत्पन्न होती है। ये प्रूडोरैंडम संख्या जेनरेटर (PRNGs) निर्धारित होते हैं और संख्या उत्पादन की श्रृंखला शुरू करने के लिए एक बीज मान की आवश्यकता होती है। बीज का चुनाव अनिश्चितता की गुणवत्ता के लिए महत्वपूर्ण है, जो क्यों वर्तमान समय का उपयोग एक सामान्य अभ्यास है।

इतिहास में, Lua की अनिश्चित संख्या उत्पादन क्षमताओं में विकास हुआ है। पहले के संस्करणों ने C मानक पुस्तकालय के `rand()` फ़ंक्शन पर निर्भर किया, जो विभिन्न कार्यान्वयनों में गुणवत्ता और प्रदर्शन में भिन्न होता था। Lua का वर्तमान संस्करण इसे और अधिक शक्तिशाली तंत्रों का उपयोग करके संभवतः सुधारता है, जो अंतर्निहित मंच पर निर्भर करता है, अनिश्चित संख्याओं का उत्पादन करने में बेहतर सामंजस्य और उपयोगिता प्रदान करता है।

प्रोजेक्ट्स के लिए जो क्रिप्टोग्राफिक-स्तर की अनिश्चितता की आवश्यकता होती है, बिल्ट-इन Lua फंक्शनालिटी काफी नहीं हो सकती है क्योंकि PRNGs की निर्धारित प्रकृति के कारण। ऐसे मामलों में, प्रोग्रामर अक्सर बाहरी लाइब्रेरीज या सिस्टम-विशिष्ट APIs की ओर मुड़ते हैं, जो उच्च-सुरक्षा अनुप्रयोगों के लिए उपयुक्त निर्धारित नहीं की गई अनिश्चित संख्याएं प्रदान कर सकते हैं।
