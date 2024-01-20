---
title:                "एक स्ट्रिंग से तारीख पार्स करना"
html_title:           "C++: एक स्ट्रिंग से तारीख पार्स करना"
simple_title:         "एक स्ट्रिंग से तारीख पार्स करना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/parsing-a-date-from-a-string.md"
---

{{< edit_this_page >}}

# Parse डेट स्ट्रिंग का उपयोग JavaScript में कैसे करें (Parsing a Date String in Javascript)

## क्या & क्यों?(What & Why?)
डेट स्ट्रिंग पार्स करने यानी एक तारीख़ को स्ट्रिंग से निकालना होता है। यह कार्य कोडर्स तारीख़ और समय को मान्यता प्राप्त स्वरूपों में बदलने के लिए करते हैं। 

## कैसे(How to:)
```Javascript
let dateStr = "2023-09-16T14:21:00Z"
let dateObj = new Date(dateStr)
console.log(dateObj)
```
यह कोड स्ट्रिंग को डेट ऑब्जेक्ट में परिवर्तित करता है और इसे कंसोल पर प्रिंट करता है। स्वरूप का उदाहरण मान्य ISO 8601 डेट स्ट्रिंग है।

## डीप डाइव (Deep Dive)
JavaScript के पुराने वर्शन में (`ECMAScript 5` से पहले), डेट पार्स करने की क्षमता बहुत ही सीमित थी। लेकिन `ECMAScript 5` ने `Date.parse()` और `new Date(dateString)` का समर्थन शुरु किया। साथ ही आपको `moment.js` जैसे लाइब्रेरीस का भी विकल्प मिलता है यदि आपको और अधिक कार्यक्षमता चाहिए।

## देखने के लिए (See Also)
* Mozilla Developer Network (MDN) Guide: [JavaScript Dates](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Numbers_and_dates)
* Moment.js: [Parse String](https://momentjs.com/docs/#/parsing/string/)
* ECMAScript specification: [Date Objects](https://www.ecma-international.org/ecma-262/5.1/#sec-15.9)