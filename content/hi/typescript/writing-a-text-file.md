---
title:                "एक टेक्स्ट फ़ाइल लिखना"
html_title:           "TypeScript: एक टेक्स्ट फ़ाइल लिखना"
simple_title:         "एक टेक्स्ट फ़ाइल लिखना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/writing-a-text-file.md"
---

{{< edit_this_page >}}

What & Why?

लेखकों जब भी अपने कोड में डेटा को स्थायित्वपूर्ण रूप से संग्रहीत रखना चाहते हैं, तो वे टेक्स्ट फाइलों का उपयोग करते हैं। टेक्स्ट फाइलों में निर्दिष्ट फॉर्मेट में डेटा रखा जाता है, जो कि प्रोग्रामरों को अपने कोड में सहेजने, संशोधन और पुनर्प्रयोग करने के लिए सहायता करता है। 

How to:

टेक्स्ट फाइल लिखने के लिए, हम सबसे पहले फ़ाइल सिस्टम को अपने कोड में लौटाते हैं: 

```TypeScript
var fs = require('fs');
```

फिर हम ```fs.writeFile()``` फ़ंक्शन का उपयोग करके नया फाइल बनाते हैं और उसमें डेटा लिखते हैं: 

```TypeScript
fs.writeFile('output.txt', 'Hello World!', function (err) {
  if (err) throw err;
  console.log('File created!');
});
```

इसके बाद, हम फाइल को सहेजते हैं और स्थानांतरित करते हैं: 

```TypeScript
fs.writeFile('output.txt', './output_files/output.txt', function (err) {
  if (err) throw err;
  console.log('File saved and moved!');
});
```

अंत में, हम इसे पढ़ते हैं और कन्सोल में प्रिंट करते हैं: 

```TypeScript
fs.readFile('./output_files/output.txt', function (err, data) {
  if (err) throw err;
  console.log(data.toString());
});
```

चाहे आप फ़ाइल सिस्टम का उपयोग करके नई फाइल बना सकते हैं, या पहले से मौजूदा फाइल में डेटा लिख सकते हैं, आपके पास बेहतरीन तरीके हैं अपने कोड को अपने आवश्यकताओं के अनुसार प्रबंधित करने के लिए। 

Deep Dive: 

टेक्स्ट फाइलों का मूल उद्देश्य उपयोगकर्ताओं को बाहर से संग्रहीत डेटा तक पहुँचने की सुविधा देना होता है। जब प्रोग्रामर दूसरे कोड में निर्दिष्ट फाइलों से डेटा लोड करते हैं, तो यह उन्हें अपने कोड को दोहराने या उद्देश्य निर्धारित करने की आवश्यकता नहीं होती है। अलग तरह की फाइल तक पहुँच मिलने से डेटा को संरक्षित करने और उपयोग करने में आसानी होती है। 

See Also: 

आप अतिरिक्त जानकारी के लिए इन स्रोतों को देख सकते हैं: 

- [Node.js File System Module](https://nodejs.org/api/fs.html)
- [Writing Files in TypeScript](https://stackabuse.com/writing-files-with-node-js-using-the-fs-module/)
- [Node.js Docs](https://nodejs.org/docs/latest-v12.x/api/fs.html)