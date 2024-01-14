---
title:                "TypeScript: Hindi में एचटीएमएल पार्सिंग"
simple_title:         "Hindi में एचटीएमएल पार्सिंग"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/parsing-html.md"
---

{{< edit_this_page >}}

## क्यों
कभी-कभी तो हमें वेब पृष्ठों से डेटा को निकालने की जरूरत होती है, जैसे कि वेब स्क्रैपिंग या डेटा माइनिंग की आवश्यकता हो सकती है। ऐसे मामलों में, HTML को पास करना बहुत ही उपयोगी हो सकता है।

## कैसे करें
महज़ 5-10 मिनटों में हम TypeScript और Cheerio लाइब्रेरी का प्रयोग करके HTML पार्सिंग के बारे में सीखेंगे। सबसे पहले, आपको ```npm install typescript``` और ```npm install cheerio``` कमांड्स को अपनी टर्मिनल में दर्ज करना होगा। तब आपको इस वीडियो लिंक पर अधिकारिक डॉक्यूमेंटेशन से अपना लर्निंग शुरू कर सकते हैं।
```TypeScript
// इस उदाहरण में हम उदाहरण हटाओं का अनुसरण करेंगे।

import * as cheerio from 'cheerio';

// HTML डॉक्यूमेंट लोड करें
let html = '<div><h1>Hello World</h1><p>This is a paragraph</p></div>';
const $ = cheerio.load(html);

// हेडिंग के लिए जीएस मिलाओ
let heading = $('h1').text();
console.log(heading); // आउटपुट: "Hello World"

// पैराग्राफ के लिए जीएस मिलाओ
let paragraph = $('p').text();
console.log(paragraph); // आउटपुट: "This is a paragraph"
```

## गहराई में जाएं
HTML पार्सिंग का क्या सचमुच अर्थ है? इसके पीछे की विज्ञान और तकनीक क्या है? आइये हम थोड़ी गहराई में जानते हैं। HTML पार्सिंग एक प्रकार का डेटा माइनिंग है जो वेब पेज़ों से डेटा को निकालता है। यह प्रक्रिया सरल है, लेकिन बहुत सारे ऑनलाइन टूल्स भी उपलब्ध हैं जो आपको डेटा पार्सिंग का काम आसान बना देते हैं। HTML पार्सिंग आमत