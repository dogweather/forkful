---
title:                "डायरेक्टरी मौजूद है या नहीं जांचना"
html_title:           "TypeScript: डायरेक्टरी मौजूद है या नहीं जांचना"
simple_title:         "डायरेक्टरी मौजूद है या नहीं जांचना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

डायरेक्टरी मौजूद है या नहीं इसे जांचना मतलब है के क्या कोई विशेष फ़ोल्डर या डायरेक्टरी पथ मौजूद है या नहीं। प्रोग्रामर्स इसे इसलिए चेक करते हैं क्योंकि यदि पथ मौजूद नहीं होता है, तो फ़ाइल की पठन या लेखन की कोशिश त्रुटियों का कारण बन सकती है।

## कैसे करें:

`TypeScript` में, `fs` मॉड्यूल द्वारा दिए गए `existsSync` function का उपयोग करके डायरेक्टरी की मौजूदगी को जांचा जा सकता है।

```TypeScript
import * as fs from 'fs';

const directoryPath = "/path/to/directory";

if (fs.existsSync(directoryPath)) {
    console.log("डायरेक्टरी मौजूद है।");
} else {
    console.log("डायरेक्टरी मौजूद नहीं है।");
}
```
स्वाभाविक रूप से, यदि डायरेक्टरी मौजूद होती है, तो आपको "डायरेक्टरी मौजूद है।" का आउटपुट मिलेगा। एन्काउंटर डायरेक्टरी नहीं होती तो "डायरेक्टरी मौजूद नहीं है।" का आउटपुट प्राप्त होगा।

## गहराई में:

इस तकनीक का मूल उपयोग किरणेन "fs" नामक Node.js कोर मॉड्यूल से मिलता है। यदि आपके पास TypeScript की अद्यतित संस्करण है, तो आप `fs.promises` API का उपयोग करके वादा-आधारित कार्यक्रम लिख सकते हैं।

इसके अलावा, आप भी `fs.access` का उपयोग कर सकते हैं जो डायरेक्टरी की मौजूदगी की जांच के साथ-साथ पहुंचने की क्षमता की भी जांच करता है। लेकिन इसमें एक समस्या हो सकती है, इसे race condition कहते हैं, इसे विगत 'access' और 'read/write' की अंतराल में directory state में हुई किसी भी परिवर्तन से उत्पन्न हो सकती है। 

## अतिरिक्त जानकारी:

अगर आप अधिक जानकारी चाहते हैं, तो आप निम्नलिखित लिंक का पालन कर सकते हैं:

1. Node.js द्वारा प्रदान किया गया `fs.existsSync` फ़ंक्शन: [https://nodejs.org/api/fs.html#fs_fs_exists_path_callback](https://nodejs.org/api/fs.html#fs_fs_exists_path_callback)

2. `fs.promises` API: [https://nodejs.org/api/fs.html#fs_fs_promises_api](https://nodejs.org/api/fs.html#fs_fs_promises_api)

3. `fs.access` function: [https://nodejs.org/api/fs.html#fs_fs_access_path_mode_callback](https://nodejs.org/api/fs.html#fs_fs_access_path_mode_callback)