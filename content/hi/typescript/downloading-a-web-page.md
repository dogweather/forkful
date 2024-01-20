---
title:                "एक वेब पेज डाउनलोड करना"
html_title:           "Kotlin: एक वेब पेज डाउनलोड करना"
simple_title:         "एक वेब पेज डाउनलोड करना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## क्या एवं क्यों?
वेब पेज डाउनलोड करना का मतलब है कि उसे इंटरनेट से अपने कंप्यूटर में स्थानांतरित करना। प्रोग्रामर्स इसे डेटा विश्लेषण, स्क्रेपिंग या बैकअप के लिए करते हैं। 

## कैसे करें:
```TypeScript
import * as https from 'https';

function downloadWebPage(url: string): void {
  https.get(url, (res) => {
    let data = '';
    res.on('data', (chunk) => {
      data += chunk;
    });
    res.on('end', () => {
      console.log(data);
    });
  }).on('error', (err) => {
    console.error("Error: " + err.message);
  });
}

downloadWebPage("https://example.com");
```
ऊपर दिए गए कोड का उदाहरण एक वेब पेज को डाउनलोड करने का है। https://example.com वेब पेज की जानकारी हमारी कंसोल में दिखाई देगी।

## गहरी जानकारी
वेब पेज डाउनलोड करने की संकल्पना का निर्माण WWW (World Wide Web) के उदय के साथ हुआ। इसके विकल्प के रूप में, कुछ डेवलपर्स ब्राउज़र एक्सटेंशन्स या वेब स्क्रेपिंग डेक्सटॉप ऐप्स का उपयोग करते हैं। ताईयारी जानकारी को अपने स्वयं के सर्वर पर स्थानांतरित करने की विधि, आपकी आवश्यकताओं पर निर्भर करेगी। 

## अन्य उपयोगी स्रोत 
1. [Node.js डॉक्यूमेंटेशन](https://nodejs.org/dist/latest-v14.x/docs/api/)
2. [TypeScript डॉक्यूमेंटेशन](https://www.typescriptlang.org/docs/)
3. [वेब स्क्रेपिंग](https://en.wikipedia.org/wiki/Web_scraping) पर विकिपीडिया