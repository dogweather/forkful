---
title:                "तारीख को एक स्ट्रिंग में रूपांतरण करना"
html_title:           "TypeScript: तारीख को एक स्ट्रिंग में रूपांतरण करना"
simple_title:         "तारीख को एक स्ट्रिंग में रूपांतरण करना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## क्यों

दिनांक को स्ट्रिंग में रूपांतरित करने के लिए कोई व्यक्ति क्यों लगेगा, उसके पीछे का कारण है कि जोन टाइम और तारीख को प्रदर्शित करने के लिए इस्तेमाल किया जा सकता है और इससे दिनांक को स्ट्रिंग के साथ जोड़कर पेश किया जा सकता है।

## कैसे करें

```TypeScript
let currentDate = new Date();
console.log(currentDate.toString()); 
```
आप TypeScript में `Date` ऑब्जेक्ट का उपयोग करके पूर्व-निर्धारित अनुसूची पर वर्तमान दिनांक बना सकते हैं। फिर आप स्ट्रिंग रूप में `toString()` फंक्शन का उपयोग करके इसे कन्वर्ट कर सकते हैं। आप अपनी आवश्यकतानुसार फॉरमेट को भी सेट कर सकते हैं। इससे आपको वर्तमान दिनांक को फॉरमेट ठीक तरीके से प्रदर्शित करने में सहायता मिलेगी।

## गहराई में जाएं

JavaScript, और उससे संबंधित TypeScript भी, में दिनांक स्ट्रिंग रूप में कन्वर्ट करने के कई तरीके हैं। यह तरीकों भिन्न-भिन्न अनुसूचियों और फॉरमेट्स को भी समर्थन करते हैं। आप `toLocaleString()` फंक्शन का भी इस्तेमाल कर सकते हैं ताकि आप विभिन्न भाषाओं और अन्य विशेषताओं को भी शामिल कर सकें। इससे आपको अपने उपयोगकर्ताओं के उद्देश्यों के लिए स्थान सुविधा भी प्रदान करेगा।

## देखें भी

- [JavaScript में दिनांक के साथ काम करने के तरीके](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [TypeScript दस्तावेज़ीकरण: Date](https://www.typescriptlang.org/docs/handbook/2/classes.html#date)
- [TypeScript Playground](https://www.typescriptlang.org/play