---
title:                "एचटीएमएल विश्लेषण"
html_title:           "TypeScript: एचटीएमएल विश्लेषण"
simple_title:         "एचटीएमएल विश्लेषण"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/parsing-html.md"
---

{{< edit_this_page >}}

## क्यों

HTML पार्सिंग क्यों करना उपयोगी हो सकता है? HTML परिवर्तन विन्यास करने या आवश्यक डेटा को उपयोग करने के लिए।

## कैसे करें

```TypeScript
// HTML पार्सिंग के लिए उपयोगी पुस्तकालय वार्तालाप करें
import * as parser from 'htmlparser2';

// HTML से डेटा प्राप्त करें
const htmlData = '<body><h1>Hello World!</h1></body>';
// HTML पार्सर बनाएं
const parsedData = parser.parseDOM(htmlData);
// चाहे स्ट्रिंग या ऑब्जेक्ट फॉर्मैट में डेटा आउटपुट करें
console.log(parsedData);
```

## डीप डाइव

HTML पार्सिंग में क्या ध्यान देना चाहिए? आपको सुनिश्चित करना होगा कि आपका पार्सिंग बिल्कुल सही रूप से काम कर रहा है और आप उचित विन्यास के लिए समर्थन दे रहे हैं। आपको अपने प्रोजेक्ट के लिए सबसे उपयुक्त पुस्तकालय चुनने की आवश्यकता हो सकती है, जो आपके कोड को अधिक सरल बनाता है।

## देखिये भी

- [HTML पार्सिंग के बारे में विवरण](https://www.tutorialspoint.com/html/html_parsing.htm)
- [htmlparser2 पुस्तकालय का आधिकारिक डॉक्यूमेंटेशन](https://github.com/fb55/htmlparser2/wiki)
- [HTML प्रोसेसिंग लाइब्रेरी का उपयोग](https://www.html5rocks.com/en/tutorials/internals/howbrowserswork/#The_Browser_Interpreter_Language_Processors)