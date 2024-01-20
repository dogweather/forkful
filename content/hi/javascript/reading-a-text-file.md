---
title:                "एक पाठ फ़ाइल पढ़ना"
html_title:           "Bash: एक पाठ फ़ाइल पढ़ना"
simple_title:         "एक पाठ फ़ाइल पढ़ना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/reading-a-text-file.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

टेक्स्ट फ़ाइल पढ़ना मतलब उसमें संग्रहीत डेटा को पढ़ना और उसे अपने कार्यक्रम में उपयोग करना। प्रोग्रामर इसे क्यों करते हैं? टेक्स्ट फ़ाइलों का डेटा कार्यक्रमों के फ़ंक्शनलिटी को बढ़ाने और मनुश्य-रीडेबल डेटा के साथ काम करने के लिए उपयोग किया जाता है। 

## कैसे करें:

```Javascript
const fs = require('fs');

fs.readFile('example.txt', 'utf8', function(err, data) {
    if (err) throw err;
    console.log(data);
});
```
इस कोड स्ट्रिप का निर्गमन आपकी `example.txt` फ़ाइल की सामग्री के आधार पर होगा।

## गहराई में:

(1) ऐतिहासिक संदर्भ: जावास्क्रिप्ट की फ़ाइल सिस्टम (fs) मॉड्यूल ने प्रोग्रामरों को फ़ाइलों के साथ काम करने का एक सरल तरीका प्रदान किया है। 

(2) विकल्प: अन्य भाषाओं में, जैसे कि Python और Ruby, भी टेक्स्ट फ़ाइल पढ़ने के लिए मॉड्यूल्स होते हैं। 

(3) क्रियान्वयन विवरण: `fs.readFile` एक असिंक्रोनस फ़ंक्शन है जो सभी IO ऑपरेशंस के दौरान उपयोगी हो सकता है, इसका अर्थ है कि इसे उपयोग करने से कार्यक्रम ब्लॉक नहीं होगा। 

## और देखें:

1. [Node.js fs.readFile() method](https://nodejs.org/api/fs.html#fs_fs_readfile_path_options_callback)
2. [Using fs.readFile in Node.js](https://stackabuse.com/read-files-with-node-js/)