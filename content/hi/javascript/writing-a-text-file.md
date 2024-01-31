---
title:                "टेक्स्ट फाइल लिखना"
date:                  2024-01-19
html_title:           "Bash: टेक्स्ट फाइल लिखना"
simple_title:         "टेक्स्ट फाइल लिखना"

category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

टेक्स्ट फाइल बनाना सीधा सा काम है: यह एक फाइल में टेक्स्ट डालने की प्रक्रिया है। प्रोग्रामर्स यह इसलिए करते हैं क्योंकि वे डेटा को सेव करना चाहते हैं, लॉग फाइल बनाना चाहते हैं, या यूज़र्स के डेटा को एक्सपोर्ट करने के लिए।

## कैसे करें:

Node.js में `fs` मॉड्यूल का इस्तेमाल करके टेक्स्ट फाइल लिखना:

```javascript
const fs = require('fs');

let data = "यह टेक्स्ट हमारी नई फाइल में सेव होगा।";

fs.writeFile('example.txt', data, (err) => {
    if (err) throw err;
    console.log('फाइल सेव हो गई है!');
});
```

इससे `example.txt` नाम की फाइल में टेक्स्ट सेव हो जाएगा।

## गहराई से जानकारी:

टेक्स्ट फाइल लिखने की सुविधा कई दशकों से है। शुरुआती दिनों में कम्पांडलाइन टूल्स और सिम्पल एडिटिंग प्रोग्राम्स का इस्तेमाल होता था। `fs` मॉड्यूल Node.js के साथ आता है और इसमें `writeFile`, जो कि असिंक्रोनस है, और `writeFileSync`, जो सिंक्रोनस है, समेत कई फंक्शंस होते हैं। 

Node.js के अलावा, वेब ब्राउज़र API में `Blob` और `FileWriter` जैसे विकल्प हैं जो क्लाइंट-साइड पर फाइल्स बना सकते हैं। 

## संदर्भ के लिए:

- Node.js `fs` मॉड्यूल डॉक्युमेंटेशन: [Node.js fs Documentation](https://nodejs.org/api/fs.html)
- वेब ब्राउज़र File API: [MDN Web Docs - File API](https://developer.mozilla.org/en-US/docs/Web/API/File)
