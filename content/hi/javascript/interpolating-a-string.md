---
title:                "स्ट्रिंग का अंतर्कलन"
html_title:           "Arduino: स्ट्रिंग का अंतर्कलन"
simple_title:         "स्ट्रिंग का अंतर्कलन"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/interpolating-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों? - What & Why?
(1) वाण का अंतर्क्रमण एक प्रक्रिया है जिसके द्वारा प्रोग्रामर प्रेक्षित मूल्यों को वाण में सम्मिलित करते हैं। (2) इसे प्रोग्रामर कम संख्या में कोड लाइन्स निर्माण करने और कोड को साफ़ और पठनीय बनाने के लिए करते हैं।

## कैसे: - How to: 
```Javascript
let name = "Rahul";
let welcomeMessage = `नमस्ते, ${name}!`;
console.log(welcomeMessage); 
```
यदि आप ऊपरी कोड का निष्पादन करते हैं, तो आपको "नमस्ते, Rahul!" इस संदेश का परिणाम मिलेगा। 

## गहरी गोता - Deep Dive
(1) वाण अंतर्क्रमण का ऐतिहासिक प्रासंगिकता: यह शैली ECMAScript 2015, जिसे आमतौर पर ES6 के नाम से जाना जाता है, के साथ जावास्क्रिप्ट में प्रवेश करी 
(2) विकल्प: वाण अंतर्क्रमण के बिना, प्रोग्रामर्स संवीदानशील वाक्यांशों ("+ var +") या वाण को जोड़ने के लिए उपयोग कर सकते हैं|
(3) आधारभूत अंतर्क्रमण विवरण: जैसा कि ${} के द्वारा, यह जावास्क्रिप्ट एक्स्प्रेशनस को मानने के लिए चाहते हैं, जो कि परिणाम वाण में शामिल होते हैं।

## देखें भी - See Also
1. [MDN Web Docs - Template Literals](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)
2. [W3Schools - Javascript String Interpolation](https://www.w3schools.com/js/js_string_templates.asp)