---
title:    "Javascript: दो तारीखों की तुलना करना"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## क्यों
दो तारीखों को तुलना करने में शामिल होने के *क्यों* को समझने के लिए, आपको निर्धारित तारीखों के बीच अंतर को जानने की जरूरत होती है। यह आपको अपने वेब साइट पर विशेष तारीखों के लिए संग्रहीत डेटा जाँचने और प्रबंधित करने में मदद कर सकता है।

## कैसे करें
"```Javascript
// दो तारीखों का अंतर कैसे निकालें
let firstDate = new Date('2021-01-01');
let secondDate = new Date('2020-12-25');

let differenceInDays = (firstDate - secondDate) / (1000 * 3600 * 24);
console.log(differenceInDays); // 7

// तीन तारीखों में से सबसे छोटा कैसे ढूंढें
let dates = [new Date('2021-01-01'), new Date('2020-12-25'), new Date('2020-12-31')];
let smallestDate = dates.reduce(function(a, b) {
    return a < b ? a : b;
});
console.log(smallestDate); // 2020-12-25
```"

## गहराई में जाइए
दो तारीखों को तुलना करना आसान है, लेकिन आप इसमें और गहराई जा सकते हैं। JavaScript में `Date` ऑब्जेक्ट को और अधिक अधिक्षण करने के लिए आप समय संबंधी ऑब्जेक्ट्स के साथ काम कर सकते हैं, जैसे `getHours()`, `getMinutes()`, और `getSeconds()`। आप अपने कोड में इन मेथड्स का उपयोग करके अन्य धाराओं को समझ सकते हैं जो आपको अंतर की गहराई में ले उतारेंगे। इसके अलावा, आप मानचित्रण और उपयोगकर्ता के लिए सही समय की गणना करने के लिए Moment.js जैसे लाइब्रेरी का उपयोग कर सकते हैं।

## देखें भी
- [JavaScript में तारीख और समय का उपयोग कैसे करें](https://www.w3schools.com/js/js_dates.asp)
- [Moment.js लाइब्रेरी की गाइड](https://momentjs.com/docs/)
- [JavaScript को प्रोग्रामिंग कर