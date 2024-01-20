---
title:                "HTML पार्स करना"
html_title:           "C++: HTML पार्स करना"
simple_title:         "HTML पार्स करना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/parsing-html.md"
---

{{< edit_this_page >}}

## "क्या और क्यों?"

HTML पार्सिंग, एचटीएमएल कोड को उसके संघटकों में तोड़ने की प्रक्रिया होती है ताकि हम उस पर काम कर सकें। कार्यक्रमकर्ताओं को इसकी आवश्यकता होती है ताकि वे हमेशा HTML में डेटा और सूचना का पता लगा सकें, और इन जानकारियों को उपयोग करके वेब एप्लिकेशनों को बेहतर बनाने के लिए कोड कर सकें।

## "कैसे करें:" 

```TypeScript
import { parse } from 'fast-html-parser';

const html = '<div class="myClass">Hello World!</div>';

let root = parse(html);

console.log(root.querySelector('.myClass').rawText); 
// Output: Hello World!
```
इस कोड का उपयोग करके, हम HTML दस्तावेज़ को पार्स कर सकते हैं और फिर हमें जो तत्व चाहिए होता है उसे ढूँढ़ने के लिए ऐसी querySelector जैसी मेथड का उपयोग कर सकते हैं।

## "गहरी जानकारी"

HTML पार्सिंग का पहला उदाहरण 1990 के दशक के बाद से देखा जा सकता है, जब WWW खोलने के बाद पहली बार इसकी आवश्यकता पाई गई।

'fast-html-parser' के अलावा भी, 'htmlparser2', 'cheerio' जैसे अन्य लाइब्रेरी भी हैं जो HTML पार्सिंग की सहायता करते हैं।

पार्सर का काम HTML को DOM (Document Object Model) में बदलना होता है। DOM एक ऑब्जेक्ट-आधारित मॉडल होता है जिसमें HTML दस्तावेज़ को नोड्स और ऑब्जेक्ट्स के रूप में दर्शाया जाता है।

## "देखने के लिए और"

ज्यादा जानकारी के लिए, नीचे दिए गए स्रोतों की जांच करें:

- [Fast HTML Parser](https://www.npmjs.com/package/fast-html-parser)
- [Cheerio.js](https://cheerio.js.org/)
- [htmlparser2](https://www.npmjs.com/package/htmlparser2)