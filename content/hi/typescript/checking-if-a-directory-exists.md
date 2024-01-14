---
title:                "TypeScript: डायरेक्ट्री मौजूद होने की जांच करें"
programming_language: "TypeScript"
category:             "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## क्यों

जब हम कोई नया प्रोजेक्ट शुरू करते हैं तो उसमें कई फोल्डर और फ़ाइलें बनाने पड़ते हैं। यदि हम उनमें से कोई फोल्डर या फ़ाइल भूल जाते हैं, तो हमारे प्रोजेक्ट को काम करने में कठिनाई आ सकती है। इसलिए हमें यह जांचने की ज़रूरत होती है कि क्या दिए गए डायरेक्टरी में हमारे द्वारा चाहे गए फोल्डर या फ़ाइलें मौजूद हैं या नहीं। यह जांच करने के लिए हम निम्नलिखित तरीकों का उपयोग कर सकते हैं। 

## कैसे करें

```TypeScript
// Importing the fs module
import * as fs from 'fs';
// Storing the directory path in a variable
let directoryPath = 'C:/Users/User/Desktop/MyProject';
// Checking if the directory exists
if (fs.existsSync(directoryPath)) {
  console.log('Directory exists');
} else {
  console.log('Directory does not exist');
}

// Output if directory exists: Directory exists
// Output if directory does not exist: Directory does not exist
```

टाइपस्क्रिप्ट में डायरेक्टरी की जांच के लिए हमें `fs` मॉड्यूल का इंपोर्ट करना होता है। उसके बाद हमें जो डिरेक्टरी का पथ हमारे पास होता है, उसे एक वेरिएबल में स्टोर करना होता है। `existsSync()` फ़ंक्शन के द्वारा हम पथ के साथ `if` कंडीशन लगाकर डायरेक्टरी की जांच कर सकते हैं। अगर डायरेक्टरी मौजूद होती है तो `existsSync()` फ़ंक्शन `true` रिटर्न करेगी और हम `Directory exists` का आउटपुट प्रिंट करेंगे, जबकि अगर डायरेक्टरी मौजूद न होती होती है तो `existsSync()` फ़ंक्शन `false` रिटर्न करेगी और हम `Directory does not exist` का आउटपुट प्रिंट करेंगे। 

## गहराई में जाओ

टाइपस्क्रिप्ट में डायरेक्ट