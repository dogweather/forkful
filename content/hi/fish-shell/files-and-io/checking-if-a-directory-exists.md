---
changelog:
- 2024-02-03, gpt-4-0125-preview, translated from English
date: 2024-02-03 19:08:13.725021-07:00
description: "\u0915\u0948\u0938\u0947: Fish Shell \u092B\u093C\u093E\u0907\u0932\
  \ \u092A\u094D\u0930\u0915\u093E\u0930\u094B\u0902 \u0914\u0930 \u0935\u093F\u0936\
  \u0947\u0937\u0924\u093E\u0913\u0902 \u0915\u0940 \u091C\u093E\u0901\u091A \u0915\
  \u0947 \u0932\u093F\u090F `test` \u0915\u092E\u093E\u0902\u0921 \u0915\u093E \u0909\
  \u092A\u092F\u094B\u0917 \u0915\u0930\u0924\u093E \u0939\u0948, \u091C\u093F\u0938\
  \u092E\u0947\u0902 \u092F\u0939 \u091C\u093E\u0901\u091A\u0928\u093E \u0936\u093E\
  \u092E\u093F\u0932 \u0939\u0948 \u0915\u093F \u0915\u094B\u0908 \u0932\u0915\u094D\
  \u0937\u094D\u092F \u0928\u093F\u0930\u094D\u0926\u0947\u0936\u093F\u0915\u093E\
  \ \u0939\u0948 \u092F\u093E \u0928\u0939\u0940\u0902\u0964\u2026"
lastmod: '2024-03-13T22:44:53.091518-06:00'
model: gpt-4-0125-preview
summary: "Fish Shell \u092B\u093C\u093E\u0907\u0932 \u092A\u094D\u0930\u0915\u093E\
  \u0930\u094B\u0902 \u0914\u0930 \u0935\u093F\u0936\u0947\u0937\u0924\u093E\u0913\
  \u0902 \u0915\u0940 \u091C\u093E\u0901\u091A \u0915\u0947 \u0932\u093F\u090F `test`\
  \ \u0915\u092E\u093E\u0902\u0921 \u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\
  \u0930\u0924\u093E \u0939\u0948, \u091C\u093F\u0938\u092E\u0947\u0902 \u092F\u0939\
  \ \u091C\u093E\u0901\u091A\u0928\u093E \u0936\u093E\u092E\u093F\u0932 \u0939\u0948\
  \ \u0915\u093F \u0915\u094B\u0908 \u0932\u0915\u094D\u0937\u094D\u092F \u0928\u093F\
  \u0930\u094D\u0926\u0947\u0936\u093F\u0915\u093E \u0939\u0948 \u092F\u093E \u0928\
  \u0939\u0940\u0902\u0964 \u092F\u0939\u093E\u0901 \u090F\u0915 \u092C\u0941\u0928\
  \u093F\u092F\u093E\u0926\u0940 \u092A\u0948\u091F\u0930\u094D\u0928 \u0939\u0948\
  \ \u091C\u093F\u0938\u0915\u093E \u0909\u092A\u092F\u094B\u0917 \u0915\u0930\u0915\
  \u0947 \u091C\u093E\u0901\u091A\u093E \u091C\u093E \u0938\u0915\u0924\u093E \u0939\
  \u0948 \u0915\u093F \u0915\u094B\u0908 \u0928\u093F\u0930\u094D\u0926\u0947\u0936\
  \u093F\u0915\u093E \u092E\u094C\u091C\u0942\u0926 \u0939\u0948 \u092F\u093E \u0928\
  \u0939\u0940\u0902."
title: "\u0921\u093E\u092F\u0930\u0947\u0915\u094D\u091F\u0930\u0940 \u092E\u094C\u091C\
  \u0942\u0926 \u0939\u0948 \u092F\u093E \u0928\u0939\u0940\u0902 \u091C\u093E\u0901\
  \u091A\u0928\u093E"
weight: 20
---

## कैसे:
Fish Shell फ़ाइल प्रकारों और विशेषताओं की जाँच के लिए `test` कमांड का उपयोग करता है, जिसमें यह जाँचना शामिल है कि कोई लक्ष्य निर्देशिका है या नहीं। यहाँ एक बुनियादी पैटर्न है जिसका उपयोग करके जाँचा जा सकता है कि कोई निर्देशिका मौजूद है या नहीं:

```fish
if test -d /path/to/dir
    echo "निर्देशिका मौजूद है"
else
    echo "निर्देशिका मौजूद नहीं है"
end
```
नमूना उत्पादन:
```
निर्देशिका मौजूद है
```

अधिक सुविधाजनक फ़ाइल और निर्देशिका ऑपरेशन के लिए, किसी भी व्यक्ति का ध्यान `fd` जैसे बाह्य उपकरणों की ओर जा सकता है, हालांकि यह अधिकतर फ़ाइलों और निर्देशिकाओं को ढूँढने के लिए प्रयोग किया जाता है, न कि केवल मौजूदगी की जाँच के लिए। हालाँकि, इसे Fish स्क्रिप्टिंग के साथ जोड़ने से उपयोगी परिणाम मिल सकते हैं:

```fish
set dir "/path/to/search"
if fd . $dir --type directory --max-depth 1 | grep -q $dir
    echo "निर्देशिका मौजूद है"
else
    echo "निर्देशिका मौजूद नहीं है"
end
```

यह `fd` उदाहरण निर्दिष्ट गहराई पर निर्देशिका की खोज करता है, और `grep` मेल की जाँच करता है, जिससे यह सूक्ष्म जाँच के लिए बहुमुखी बनता है। हालाँकि, केवल मौजूदगी की जाँच के प्रत्यक्ष उद्देश्य के लिए, Fish के निर्मित `test` का उपयोग करना दोनों ही दृष्टिकोण से कुशल और सरल है।
