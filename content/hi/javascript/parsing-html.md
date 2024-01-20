---
title:                "HTML पार्स करना"
html_title:           "C++: HTML पार्स करना"
simple_title:         "HTML पार्स करना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/parsing-html.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

HTML पार्सिंग, ह्यूमन रीडेबल टेक्स्ट को मशीन रीडेबल आकार मे बदलने का काम हैं। इसे प्रोग्रामर्स तब करते हैं जब उन्हें वेब पेज से डाटा निकालने की आवश्यकता होती हैं।

## कैसे करें:

```Javascript
let parser = new DOMParser();
let doc = parser.parseFromString('<html><body><p>Hello, World!</p></body></html>', 'text/html');
console.log(doc.body.textContent);
```

उपरोक्त कोड साम्पल का ऑउटपुट होगा:

```Javascript
'Hello, World!'
```

## गहराई में:

HTML पार्सिंग के इतिहास में, और भी मेथड्स का उपयोग किया जाता था किंतु DOMParser अब सबसे अधिक प्रयोग की जाने वाली विधि है। सामान्य तौर पर इससे बेहतर प्रदर्शन प्राप्त होता है और यह वेब ब्राउज़र के संगतता के लिए बेहतर होता है। इसके विकल्प स्वरंप जैसे regex हो सकते हैं लेकिन वे कठिन हो सकते हैं और सटीकता में कमी हो सकती है।

HTML पार्सिंग का एक और महत्वपूर्ण पहलु है DOM का निर्माण - यह एक इंटरैक्टिव रेप्रिजेंटेशन होता है जिसमें कोई तत्व गर्मण कर सकता है, इसलिए कोड के साथ इंटरैक्ट कर सकता है।

## देखें भी:

1. [Mozilla Developer Network (MDN) DOM Parsing Guide](https://developer.mozilla.org/en-US/docs/Web/API/DOMParser): डितेल्ड गाइड और मेथड एप्लीकेशन का विवरण।
2. [W3Schools HTML DOM Parser](https://www.w3schools.com/js/js_htmldom_parser.asp): मूलभूत गाइड और एग्जाम्पल।
3. [HTML Parser by Node.js](https://nodejs.org/api/html.html): Node.js का HTML पार्सर डॉक्युमेंटेशन।