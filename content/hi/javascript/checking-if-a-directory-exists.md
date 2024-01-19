---
title:                "एक निर्देशिका मौजूद है या नहीं यह जांचना"
html_title:           "Elixir: एक निर्देशिका मौजूद है या नहीं यह जांचना"
simple_title:         "एक निर्देशिका मौजूद है या नहीं यह जांचना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

# Javascript में Directory मौजूद है या नहीं कैसे चेक करें

## क्या और क्यों?

डायरेक्टरी मौजूद है या नहीं, इसकी जांच करना मतलब होता है कि क्या कोई खास फ़ाइल सिस्टम पथ पर एक फ़ोल्डर मौजूद है या नहीं। प्रोग्रामर्स इसे उन त्रुटियों को रोकने के लिए करते हैं जो तभी उत्पन्न होती हैं जब वे किसी अस्तित्वहीन डायरेक्टरी से उपर काम करने का प्रयास करते हैं।

## कैसे करें:

Node.js वातावरण में, आप `fs` मॉड्यूल के `existsSync()` फ़ंक्शन का उपयोग करके एक डायरेक्टरी की जांच कर सकते हैं। 

```Javascript
var fs = require('fs');
if (fs.existsSync('/निर्दिष्ट/पथ')) {
    console.log('डायरेक्टरी मौजूद है');
} else {
    console.log('डायरेक्टरी मौजूद नहीं है');
}
```

ऊपर दिए गए कोड के आउटपुट में, या तो "डायरेक्टरी मौजूद है" या "डायरेक्टरी मौजूद नहीं है" होगा, यह उस पथ पर डायरेक्टरी की मौजूदगी पर निर्भर करेगा।

## गहराई में:

`fs.existsSync()`  हिस्ट्रिकली Node.js के `fs` मॉड्यूल का हिस्सा रहा है। इसके विकल्प स्वरूप, आप `fs.stat()` या `fs.access()` का उपयोग कर सकते हैं, परन्तु `existsSync()` आपको ऐसी साधारणता और संक्षिप्तता प्रदान करती है जो अन्य विधियों से कम होती है। 

अन्य भाषाओं में, डायरेक्टरी के अस्तित्व की जांच के लिए भिन्न आवश्यकताएं हो सकती हैं. प्रायः, इसे आपरेटिंग सिस्टम API कॉल के माध्यम से किया जाता है, जो पथ की जांच करता है।

## देखें भी:

- Node.js डॉक्स के `fs` मॉड्यूल ([स्रोत](https://nodejs.org/api/fs.html))
- StackOverflow पर "Check if directory exists in Node.js" ([स्रोत](https://stackoverflow.com/questions/11458068/how-to-check-if-a-directory-or-a-file-exists-in-system-or-not-using-node-js))
- "Understanding fs module in Node.js" ([स्रोत](https://www.geeksforgeeks.org/understanding-fs-module-in-node-js/))