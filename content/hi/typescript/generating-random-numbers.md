---
title:                "यादृच्छिक संख्याओं का निर्माण"
html_title:           "Clojure: यादृच्छिक संख्याओं का निर्माण"
simple_title:         "यादृच्छिक संख्याओं का निर्माण"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/generating-random-numbers.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
रैंडम नंबर उत्पन्न करना का मतलब होता है एक बेतरतीब (अप्रत्याशित) संख्या उत्पन्न करना। कार्यक्रमकर्ताएं यदि संख्या का अनुमान लगाना चाहते हैं, लेकिन उन्हें पता नहीं होता कि वो कोनसी होगी, तो वे रैंडम नंबर उत्पन्न करते हैं।

## कैसे बनाएं: 
आइए देखें कुछ टाइपस्क्रिप्ट कोड का नमूना:

```TypeScript
function getRandomArbitrary(min: number, max: number) {
  return Math.random() * (max - min) + min;
}

console.log(getRandomArbitrary(1, 100));
```
इस कोड से आप निम्न और ऊपर सीमा के बीच में एक उत्कृष्ट रैंडम संख्या उत्पन्न कर सकते हैं। साउटपुट कुछ इस तरह होगा:

`56.78996221962878`

## गहरी जानकारी
रैंडम नंबर जनरेशन का इतिहास बहुत रोमांचक है, जिसमें ज्यामिति और संख्या विज्ञान का मजेदार संगम है। 

वैकल्पिक तरीके में, आप Math.random() की जगह crypto.getRandomValues() का उपयोग कर सकते हैं, जो और अधिक यादृच्छिक और सुरक्षित होता है। लेकिन, यह ब्राउज़र के वातावरण में ही काम करता है।

Math.random() आपको 0 (समेत) और 1 (लिए बिना) के बीच एक यादृच्छिक संख्या देता है। इसलिए, हमने इसे min और max के बीच अपने संख्याओं की सीमा मिलाने के लिए उपयोग किया।

## अधिक जानें 
[RNG विकिपीडिया पृष्ठ](https://en.wikipedia.org/wiki/Random_number_generation)
[MDN Web Docs Math.random()](https://developer.mozilla.org/en/docs/Web/JavaScript/Reference/Global_Objects/Math/random)
[MDN Web Docs crypto.getRandomValues()](https://developer.mozilla.org/en-US/docs/Web/API/Crypto/getRandomValues)