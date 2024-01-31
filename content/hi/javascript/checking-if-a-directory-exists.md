---
title:                "डायरेक्टरी का अस्तित्व जाँचना"
date:                  2024-01-20T14:57:34.683191-07:00
html_title:           "Elm: डायरेक्टरी का अस्तित्व जाँचना"
simple_title:         "डायरेक्टरी का अस्तित्व जाँचना"

category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
डायरेक्टरी का वजूद चेक करना मतलब है पता लगाना कि कोई फोल्डर मौजूद है या नहीं। प्रोग्रामर्स इसे इसलिए करते हैं क्योंकि कोड को सही से चलाने के लिए जरूरी है कि संबंधित फोल्डर्स मौजूद हों।

## How to: (कैसे करें:)
```Javascript
const fs = require('fs');

// डायरेक्टरी के वजूद को चेक करने का सिंक्रोनस तरीका
let directoryExists = fs.existsSync('/path/to/directory');
console.log(directoryExists); // अगर डायरेक्टरी है, तो true, नहीं तो false

// डायरेक्टरी को असिंक्रोनस तरीके से चेक करना
fs.stat('/path/to/directory', (err, stats) => {
  if(err && err.code === 'ENOENT') {
      console.log(false); // डायरेक्टरी नहीं मिली
  } else {
      console.log(stats.isDirectory()); // अगर डायरेक्टरी है, तो true
  }
});
```

## Deep Dive (गहराई से जानकारी)
नोड.जेएस में `fs` मॉड्यूल का उपयोग करके हमें फाइल सिस्टम का एक्सेस मिलता है। `fs.existsSync` और `fs.stat` दो मेथड्स हैं जो डायरेक्टरी के वजूद को चेक करते हैं। `fs.existsSync` सिंक्रोनस है, जिससे कोड ब्लॉक हो सकता है, इसलिए जहां परफॉर्मेंस महत्वपूर्ण हो, वहां `fs.stat` का असिंक्रोनस वर्जन बेहतर रहता है। बीते सालों में, इस फीचर का इस्तेमाल फाइल सिस्टम की सुरक्षा और इंटेग्रिटी को बनाए रखने के लिए किया जाता रहा है। विकल्प के रूप में, लाइब्रेरीज जैसे कि `fs-extra` और `fs-promise` भी मौजूद हैं जो अधिक उन्नत फंक्शनालिटी और प्रोमिस बेस्ड एपीआई प्रदान करते हैं।

## See Also (और देखें)
- [Node.js fs Docs](https://nodejs.org/api/fs.html)
- [fs-extra npm Package](https://www.npmjs.com/package/fs-extra)
- [fs-promise npm Package](https://www.npmjs.com/package/fs-promise)
