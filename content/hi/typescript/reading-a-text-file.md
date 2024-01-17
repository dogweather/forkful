---
title:                "एक पाठ फाइल को पढ़ना"
html_title:           "TypeScript: एक पाठ फाइल को पढ़ना"
simple_title:         "एक पाठ फाइल को पढ़ना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
पाठ फाइल पढने का मतलब है किसी फाइल के साथ काम करना जो टेक्स्ट के रूप में लिखा होता है। इसे प्रोग्रामर्स इसलिए करते हैं क्योंकि वे अपने कोड में सुधार कर सकते हैं और डेटा को अपनी आवश्यकताओं के अनुसार प्रोसेस कर सकते हैं।

## कैसे करें:
```TypeScript
import * as fs from 'fs';

// फाइल पढ़ना
let data = fs.readFileSync('sample.txt', 'utf8');

// पढ़े गए डेटा का उपयोग करके कुछ अन्य ऑपरेशन करें
console.log(data);
```
कोड ब्लॉक में दिया गया उदाहरण फाइल वास्तविकता से मैच नहीं करता है, इसलिए आप अपनी फ़ाइल का नाम या रास्ता अपनी आवश्यकताओं के अनुसार बदल सकते हैं। आप फ़ाइल को 'utf8' विकल्प के साथ कहीं भी पढ़ सकते हैं। यदि आप इसे उपयोग नहीं करते हैं, तो आपको डेटा बाइनरी फॉर्म में मिलेगा। 

## गहराई में जाएं:
1. इतिहास: पाठ फाइल बकाया समय से ही प्रोग्रामिंग का अभ्यास है। पहले प्रोग्रामर्स को स्टोरेज तक पहुँचने का कोई टूल नहीं था, इसलिए वे अपने कोड में ही स्टोरेज क्रियाएं करते थे।
2. वैकल्पिक: आप fs.readFile फ़ंक्शन का भी इस्तेमाल कर सकते हैं जो आपको असिंक्रोनस रूप से डेटा पढ़ने की अनुमति देता है।
3. अनुपालन: आप fs.readFileSync के साथ कोई भी फाइल पढ़ सकते हैं, यदि वह फाइल मौजूद होती है। इससे आपको खातरा हो सकता है, इसलिए यदि आप नुकसान से बचना चाहते हैं तो हमेशा fs.existsSync का उपयोग करें।

## इससे जुड़े और:
- [Node.js API डॉक्यूमेंटेशन] (https://nodejs.org/api/fs.html)
- [क्यों fs.readFileSync के साथ तुलना में fs.readFile का उपयोग करें] (https://dev.to/oieduardorabelo/fs-readfile-vs-fs-readfilesync-37lo)
- [पाठ फाइल की पूरी जानकारी] (https://www.geeksforgeeks.org/reading-data-from-a-text-file-using-node-js/)