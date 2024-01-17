---
title:                "टेक्स्ट फाइल लिखना"
html_title:           "Javascript: टेक्स्ट फाइल लिखना"
simple_title:         "टेक्स्ट फाइल लिखना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

# क्या और क्यों? 

टेक्स्ट फ़ाइल लिखने का मतलब होता है की हम प्रोग्राम के द्वारा मौजूदात को स्टोर करते हैं । प्रोग्रामर्स इसे अपने कोड का एक अंश के रूप में रखने के लिए करते हैं । 

# कैसे करें: 

```javascript 
const fs = require('fs');

fs.writeFile('myTextFile.txt', 'This is a text file written using Javascript!', function (err) {
  if (err) throw err;
  console.log('Text file written successfully!');
});
```

इस coding example में, हमने `fs` module का उपयोग किया है जो node.js में built-in module है । हमने `writeFile()` function का उपयोग करके एक नया text file बनाया है और उसमे एक string value भी डाली है । अगर कोई error होती है तो `throw` keyword का उपयोग करके हम error को देख सकते हैं और अगर सब कुछ सही चलता है तो हम console में success message देख सकते हैं । 

# गहराई में जाएं: 

## इतिहासिक परिप्रेक्ष्य: 

पहले जब computers में storage की capacity कम थी, तब programmers इन text files को डाटाबेस के रूप में इस्तेमाल करते थे । लेकिन अब बड़ी स्तर पर इनका उपयोग कम हो गया है क्योंकि databases में डेटा को सुरक्षित और आसानी से अपडेट किया जा सकता है । 

## विकल्प: 

कुछ programming languages में, यह text file लिखने की प्रक्रिया थोड़ी अलग होती है विशेष रूप से उन languages में जो compiled नहीं होते हैं । कुछ अन्य तरीकों से भी आप text file लिख सकते हैं, जैसे database, spreadsheets, और text editors के ज़रिए । 

## गणना विवरण: 

जब हम text file लिखते हैं, हम उस file को encoding के रूप में भी specify कर सकते हैं । आमतौर पर, `utf-8` encoding के साथ तेज़ी से लिखा जाता है क्योंकि यह हर अक्षर को एक byte में convert करता है । लेकिन अगर आपको इसके बारे में और अधिक जानकारी चाहिए तो आप `fs` module की documentation देख सकते हैं । 

# इसे भी देखें: 

- [Node.js FileSystem Module](https://nodejs.org/api/fs.html)
- [Writing Files using Node.js](https://www.w3schools.com/nodejs/nodejs_filesystem.asp)