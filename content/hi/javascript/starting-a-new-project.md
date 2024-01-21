---
title:                "नई परियोजना शुरू करना"
date:                  2024-01-20T18:04:21.191578-07:00
model:                 gpt-4-1106-preview
simple_title:         "नई परियोजना शुरू करना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Getting Started"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/starting-a-new-project.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
नया प्रोजेक्ट शुरू करना मतलब कोड की एक नई दुनिया बनाना। प्रोग्रामर इसे नए आइडिया टेस्ट करने, स्किल्स सुधारने, या कुछ उपयोगी बनाने के लिए करते हैं।

## How to (कैसे करें):
```javascript
// Node.js पर नया प्रोजेक्ट बनाने के लिए:
const fs = require('fs');
const projectName = 'NewProject';

fs.mkdir(projectName, { recursive: true }, (err) => {
  if (err) throw err;
  console.log(`${projectName} directory created!`);
});

// package.json फाइल बनाएँ:
const packageJson = {
  name: projectName,
  version: '1.0.0',
  description: '',
  main: 'index.js',
  scripts: {
    test: 'echo "Error: no test specified" && exit 1'
  },
  author: '',
  license: 'ISC'
};

fs.writeFile(`${projectName}/package.json`, JSON.stringify(packageJson, null, 2), (err) => {
  if (err) throw err;
  console.log('package.json file created!');
});
```

## Deep Dive (गहराई से जानकारी):
नया JavaScript प्रोजेक्ट शुरू करते समय आपको कुछ बातों का ध्यान रखना होता है - कोड की स्ट्रक्चर, डिपेंडेंसी मैनेजमेंट, और प्रोजेक्ट की टेस्टिंग। पहले, सभी को अपने-अपने हिसाब से सेटअप करना पड़ता था, लेकिन आजकल टूल्स जैसे कि `npm` और `yarn` इस प्रक्रिया को बहुत आसान बना देते हैं। इनके साथ, आप मात्र कुछ कमांड्स के साथ एक स्टैंडर्ड प्रोजेक्ट तैयार कर सकते हैं और विभिन्न पैकेजेस जल्दी से इंस्टॉल कर सकते हैं।

डिपेंडेंसी मैनेजमेंट के लिए `package.json` का इस्तेमाल होता है, जोकि प्रोजेक्ट की जानकारी और इस्तेमाल किए गए पैकेजेस को ट्रैक करती है। इसमें उन पैकेजेस के वर्ज़न भी शामिल होते हैं जो प्रोजेक्ट के साथ काम करते हैं, और इसे `npm install` जैसे कमांड्स के जरिए मैनेज किया जाता है।

एक बार जब आपकी बेसिक सेटअप तैयार हो जाती है, तो आप अपना कोड लिखना शुरू कर सकते हैं। फिर स्क्रिप्ट्स लिखें, पैकेजेस इंस्टॉल करें, और आपका प्रोजेक्ट शेप लेने लगेगा।

## See Also (और भी जानकारी):
- [npm documentation](https://docs.npmjs.com/)
- [GitHub Guides](https://guides.github.com/)
- [JavaScript Info](https://javascript.info/)