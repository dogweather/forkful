---
date: 2024-01-27 20:34:25.854872-07:00
description: "\u0915\u0948\u0938\u0947: Fish \u092E\u0947\u0902 \u090F\u0915 \u0930\
  \u0948\u0902\u0921\u092E \u0938\u0902\u0916\u094D\u092F\u093E \u0909\u0924\u094D\
  \u092A\u0928\u094D\u0928 \u0915\u0930\u0928\u093E \u0938\u0930\u0932 \u0939\u094B\
  \ \u0938\u0915\u0924\u093E \u0939\u0948, \u0938\u093F\u0938\u094D\u091F\u092E \u0909\
  \u092A\u0915\u0930\u0923\u094B\u0902 \u0914\u0930 \u0936\u0948\u0932 \u0915\u094D\
  \u0937\u092E\u0924\u093E\u0913\u0902 \u0915\u0947 \u0938\u0902\u092F\u094B\u091C\
  \u0928 \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0924\u0947 \u0939\
  \u0941\u090F\u0964 \u0928\u0940\u091A\u0947 \u0915\u0941\u091B \u0909\u0926\u093E\
  \u0939\u0930\u0923 \u0926\u093F\u090F \u0917\u090F \u0939\u0948\u0902 \u091C\u094B\
  \u2026"
lastmod: '2024-04-05T21:53:54.994988-06:00'
model: gpt-4-0125-preview
summary: "Fish \u092E\u0947\u0902 \u090F\u0915 \u0930\u0948\u0902\u0921\u092E \u0938\
  \u0902\u0916\u094D\u092F\u093E \u0909\u0924\u094D\u092A\u0928\u094D\u0928 \u0915\
  \u0930\u0928\u093E \u0938\u0930\u0932 \u0939\u094B \u0938\u0915\u0924\u093E \u0939\
  \u0948, \u0938\u093F\u0938\u094D\u091F\u092E \u0909\u092A\u0915\u0930\u0923\u094B\
  \u0902 \u0914\u0930 \u0936\u0948\u0932 \u0915\u094D\u0937\u092E\u0924\u093E\u0913\
  \u0902 \u0915\u0947 \u0938\u0902\u092F\u094B\u091C\u0928 \u0915\u093E \u0909\u092A\
  \u092F\u094B\u0917 \u0915\u0930\u0924\u0947 \u0939\u0941\u090F\u0964 \u0928\u0940\
  \u091A\u0947 \u0915\u0941\u091B \u0909\u0926\u093E\u0939\u0930\u0923 \u0926\u093F\
  \u090F \u0917\u090F \u0939\u0948\u0902 \u091C\u094B \u0926\u0930\u094D\u0936\u093E\
  \u0924\u0947 \u0939\u0948\u0902 \u0915\u093F \u0928\u093F\u0930\u094D\u0927\u093E\
  \u0930\u093F\u0924 \u0930\u0947\u0902\u091C \u092E\u0947\u0902 \u0930\u0948\u0902\
  \u0921\u092E \u0938\u0902\u0916\u094D\u092F\u093E\u090F\u0901 \u0915\u0948\u0938\
  \u0947 \u0909\u0924\u094D\u092A\u0928\u094D\u0928 \u0915\u0940 \u091C\u093E \u0938\
  \u0915\u0924\u0940 \u0939\u0948\u0902\u0964 **0 \u0914\u0930 100 \u0915\u0947 \u092C\
  \u0940\u091A \u090F\u0915 \u0930\u0948\u0902\u0921\u092E \u0938\u0902\u0916\u094D\
  \u092F\u093E \u0909\u0924\u094D\u092A\u0928\u094D\u0928 \u0915\u0930\u0947\u0902\
  :**."
title: "\u092F\u093E\u0926\u0943\u091A\u094D\u091B\u093F\u0915 \u0938\u0902\u0916\u094D\
  \u092F\u093E\u090F\u0901 \u0909\u0924\u094D\u092A\u0928\u094D\u0928 \u0915\u0930\
  \u0928\u093E"
weight: 12
---

## कैसे:
Fish में एक रैंडम संख्या उत्पन्न करना सरल हो सकता है, सिस्टम उपकरणों और शैल क्षमताओं के संयोजन का उपयोग करते हुए। नीचे कुछ उदाहरण दिए गए हैं जो दर्शाते हैं कि निर्धारित रेंज में रैंडम संख्याएँ कैसे उत्पन्न की जा सकती हैं।

**0 और 100 के बीच एक रैंडम संख्या उत्पन्न करें:**

```fish
set -l rand_num (random 0 100)
echo $rand_num
```

**नमूना उत्पादन:**
```fish
42
```

**किसी दो संख्याओं के बीच एक रैंडम संख्या उत्पन्न करना, उदाहरण के लिए 50 और 150 के बीच:**

```fish
set -l min 50
set -l max 150
set -l rand_num (random $min $max)
echo $rand_num
```

**नमूना उत्पादन:**
```fish
103
```

**सूची को रैंडमली शफल करने के लिए रैंडम का उपयोग करना:**

आप शायद एक सूची में तत्वों को यादृच्छिक रूप से शफल करना चाहेंगे। यहाँ पर आप कैसे कर सकते हैं:

```fish
set -l my_list A B C D E
random (seq (count $my_list)) | while read i
    echo $my_list[$i]
end
```

**नमूना उत्पादन:**
```fish
C
A
E
D
B
```

कृपया ध्यान दें, यादृच्छिकता के स्वभाव के कारण इन कमांडों को चलाने पर प्रत्येक बार उत्पादन भिन्न होगा।

## गहराई से समझें
Fish Shell `random` फंक्शन यादृच्छिक संख्याएँ उत्पन्न करने के लिए एक सरल तो उपयोग इंटरफेस प्रदान करता है। आंतरिक रूप से, यह सिस्टम-स्तरीय यादृच्छिक संख्या उत्पादन उपकरणों के चारों ओर लपेटता है, आपके स्क्रिप्टों में यादृच्छिकता को शामिल करने के लिए एक पोर्टेबल तरीका प्रदान करता है। हालांकि, यह याद रखना महत्वपूर्ण है कि `random` द्वारा प्रदान की गई यादृच्छिकता अधिकांश स्क्रिप्टिंग कार्यों के लिए पर्याप्त है लेकिन उच्चतम अप्रत्याशितता डिग्री की आवश्यकता वाले एप्लिकेशनों के लिए क्रिप्टोग्राफिक सुरक्षा आवश्यकताओं को पूरा नहीं कर सकती।

उच्च-सुरक्षा संदर्भों में, क्रिप्टोग्राफिक उद्देश्यों के लिए डिज़ाइन किए गए समर्पित उपकरणों या प्रोग्रामिंग लाइब्रेरीज़ का उपयोग करने पर विचार करें, जो मजबूत यादृच्छिकता गारंटी प्रदान करते हैं। फिर भी, जहाँ यादृच्छिकता के लिए उच्चतम सुरक्षा मानक आवश्यक नहीं हैं, वहाँ फिश शैल का `random` फंक्शन एक सुविधाजनक और प्रभावी समाधान प्रदान करता है।
