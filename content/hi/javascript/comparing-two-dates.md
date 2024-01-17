---
title:                "दो तिथियों को तुलना करना"
html_title:           "Javascript: दो तिथियों को तुलना करना"
simple_title:         "दो तिथियों को तुलना करना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
किसी भी प्रोग्रामर के लिए दो तिथियों को तुलना करने का काम बहुत अहम होता है। यह उन्हें दो तारीखों के बीच का अंतर निर्धारित करने में मदद करता है।

## कैसे करें:
```Javascript
// तिथियों की तुलना करने के लिए सामान्य में इस्तेमाल किया जाने वाला तरीका है:
const date1 = new Date('2021-11-01');
const date2 = new Date('2021-10-01');

if (date1 > date2) {
    console.log('दोनों तारीखों में से बड़ी तारीख है:', date1);
} else {
    console.log('दोनों तारीखों में से बड़ी तारीख है:', date2);
}

// परिणाम:
// दोनों तारीखों में से बड़ी तारीख है: Mon Nov 01 2021 00:00:00 GMT+0530 (भारतीय मानक समय)
```

## गहराई में जाएं:
तिथियों के बीच तुलना का इतिहास बहुत दूर तक जाता है। पहले प्रोग्रामरों को तिथियों को मिलाने के लिए खाँद का इस्तेमाल करना पड़ता था। आजकल, तिथि और समय के प्रबंधन के लिए स्टैण्डर्ड जावास्क्रिप्ट लाइब्रेरी में बसा समय प्रवर्धन प्रणाली है। अन्य तरीकों में मिलाने के लिए, moment.js जेएस के लिए सबसे लोकप्रिय पैकेज है। दो तारीखों को तुलना करने के लिए, जावास्क्रिप्ट "==" और "===" ऑपरेटर भी उपयोगी हो सकते हैं, लेकिन ध्यान रखें कि वे समान संभावना में भिन्न परिणाम दे सकते हैं।

## इससे जुड़े स्रोत:
https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date

https://momentjs.com/docs/

http://www.ecma-international.org/ecma-262/5.1/#sec-11.9.3