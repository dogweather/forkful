---
title:                "Javascript: एक टेक्स्ट फ़ाइल लिखना"
simple_title:         "एक टेक्स्ट फ़ाइल लिखना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/writing-a-text-file.md"
---

{{< edit_this_page >}}

## क्यों
किसी भी व्यक्ति को विचार करना मतलब है कि क्यों कोई टेक्स्ट फ़ाइल लिखने से लाभान्वित हो सकता है।

## कैसे करें
"```Javascript
const fs = require('fs');

// यहां आपको एक नई टेक्स्ट फ़ाइल बनाने का उदाहरण दिया गया है
fs.writeFileSync('newfile.txt', 'यह एक नई टेक्स्ट फ़ाइल है!');
```
कोड ब्लॉक उल्लेखित स्क्रिप्टिंग को इम्पोर्ट करता है और `writeFileSync()` फ़ंक्शन को उपयोग करके एक नई फ़ाइल बनाता है। उसके अंदर उपयोगकर्ता अपना वह टेक्स्ट दे सकता है जो वह चाहता है कि उस फ़ाइल में लिखा जाए। इसी प्रकार की फ़ुंक्शन हैं जो टेक्स्ट फ़ाइल लिखने में सहायता कर सकती हैं।

## गहराई में
जब हम JDK, IDE आदि से भारत में दूर परिचय खोल रहे हों, तो वैसे JS (Java Scripting) की सामान्य बाकग्राउंड सरोजनामा से लगभगर हम होगे और उनके पास ऐसे प्रवेश नहीं होंगे जैसा कि यह बड़ा सवाल हो सकता है नहीं और जब ताक्की सी है कि वह मैसिज दिखता हो, यह मुफक़ो कैसी खास इनपुट हो सकता है।

## भी जानिए
[इतिहास के बारे में](https://hi.wikipedia.org/wiki/JavaScript), [लीगल विवाद](https://www.thehindu.com/sci-tech/technology/internet/what-is-javascript/article29729547.ece) और [जावास्क्रिप्ट की एक पहेली](https://www.tutorialrepublic.com/faq/what-is-the-difference-between-javascript-and-jscript.php)।