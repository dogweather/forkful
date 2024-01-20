---
title:                "एक पाठ फ़ाइल पढ़ना"
html_title:           "Bash: एक पाठ फ़ाइल पढ़ना"
simple_title:         "एक पाठ फ़ाइल पढ़ना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/reading-a-text-file.md"
---

{{< edit_this_page >}}

# TypeScript से टेक्स्ट फ़ाइल पढ़ना

## क्या और क्यों?

टेक्स्ट फ़ाइल पढ़ना मतलब है की टेक्स्ट फ़ाइल का डाटा प्रोग्रामिंग भाषा के माध्यम से प्राप्त करने की क्रिया। प्रोग्रामर इसे इसलिए करते हैं ताकि वे डाटा के साथ मनिपुलेशन कर सके और प्रोग्रामिंग सीख सके। 

## कैसे करें:

आइए देखते हैं कैसे TypeScript के साथ एक टेक्स्ट फ़ाइल पढ़ा जाता है। 

```TypeScript
const fs = require('fs')

fs.readFile('सम्पादन.txt', 'utf8' , (err, data) => {
  if (err) {
    console.error(err)
    return
  }
  console.log(data)
})
```
उपरोक्त कोड का आउटपुट सम्पादन.txt फ़ाइल की सामग्री होगी। 

## गहराई में:

### ऐतिहासिक परिप्रेक्ष्य

TypeScript कैसे टेक्स्ट फ़ाइल पढ़ता है इसका इतिहास Node.js संबंधी fs (file system) module से जुड़ा हुआ है। 

### विकल्प

TypeScript के अलावा आप JavaScript, Python आदि का उपयोग भी कर सकते हैं। 

### कार्यान्वयन विवरण

टेक्स्ट फ़ाइल को पढ़ना fs.readFile नामक function का उपयोग करके किया जाता है। 

## देखें भी:

- [Node.js fs module गाइड](https://nodejs.org/api/fs.html)
- [TypeScript टुटोरियल](https://www.typescriptlang.org/docs/)

कृपया उपरोक्त लिंक पर क्लिक करके अधिक जानकारी प्राप्त करें।