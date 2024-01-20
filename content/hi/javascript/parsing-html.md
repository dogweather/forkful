---
title:                "HTML पार्स करना"
date:                  2024-01-20T15:32:25.202418-07:00
html_title:           "Bash: HTML पार्स करना"
simple_title:         "HTML पार्स करना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/parsing-html.md"
---

{{< edit_this_page >}}

## What & Why?
HTML पार्स करना क्या है और क्यों करते हैं? यह एक ऐसी प्रक्रिया है जिसके ज़रिये हम HTML से डाटा निकालते हैं या उसे मॉडिफाई करते हैं। यह जरूरी है वेबसाइट्स की स्ट्रक्चर को समझने और वेब डाटा एनालिसिस और मैनेजमेंट के लिए।

## How to:
```Javascript
// Example: Parsing an HTML string using DOMParser
const htmlString = '<div id="greeting">नमस्ते, दुनिया!</div>';

// DOMParser का उपयोग करके HTML स्ट्रिंग को पार्स करना
const parser = new DOMParser();
const doc = parser.parseFromString(htmlString, 'text/html');

// id "greeting" वाले एलिमेंट को ढूंढना और उसका टेक्स्ट प्राप्त करना
const greetingText = doc.querySelector('#greeting').textContent;
console.log(greetingText);  // "नमस्ते, दुनिया!"
```

## Deep Dive
HTML पार्सिंग एक पुरानी अवधारणा है और इसका इतिहास वेब की शुरुआत के साथ ही जुड़ा है। `DOMParser` वर्तमान में ब्राउज़र में पार्सिंग के लिए एक स्टैंडर्ड तरीका है, पर वैकल्पिक लाइब्रेरीज जैसे `jQuery`, `cheerio` (node.js पर) भी मौजूद हैं। ये लाइब्रेरीज इस काम को आसान बनाती हैं, खासकर जब जटिल HTML संरचनाओं का सामना हो।

परफॉरमेंस की बात करें तो `DOMParser` तेज़ी से और सुरक्षित रूप से पार्स करता है, क्योंकि यह ब्राउज़र द्वारा नेटिवली सपोर्टेड होता है। इसके उपयोग में सिक्योरिटी पर विशेष ध्यान दिया जाता है, ताकि किसी भी खतरनाक स्क्रिप्ट्स से बचा जा सके जो XSS (Cross-Site Scripting) हमले का कारण बन सकती हैं।

## See Also
- MDN Web Docs on DOMParser: [DOMParser](https://developer.mozilla.org/en-US/docs/Web/API/DOMParser)
- jQuery पर HTML पार्सिंग के लिए गाइड: [jQuery.parseHTML()](https://api.jquery.com/jquery.parsehtml/)
- Node.js के लिए `cheerio` लाइब्रेरी: [Cheerio](https://cheerio.js.org/)