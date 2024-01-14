---
title:                "TypeScript: डिरेक्टरी का अस्तित्व जांचें"
simple_title:         "डिरेक्टरी का अस्तित्व जांचें"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## क्यों

सभी प्रोग्रामिंग प्रोजेक्ट में, एक डाइरेक्ट्री या फोल्डर की उपस्थिति की जांच बहुत महत्वपूर्ण हो सकती है। यदि हमें डाइरेक्ट्री के बारे में जानकारी है, तो हम उसमें फाइलें खोज सकते हैं और उन्हें प्रोसेस कर सकते हैं। इसलिए, डाइरेक्ट्री की जांच करने एक महत्वपूर्ण कार्य है और इस लेख में हम देखेंगे कि यह कैसे किया जाता है।

## कैसे करें

अगर हम डाइरेक्ट्री कोई पथ पास करते हैं, तो हम इसकी उपस्थिति की जांच कर सकते हैं इसके लिए हम "fs.existSync()" फंक्शन का उपयोग कर सकते हैं। यह फंक्शन "true" या "false" के साथ एक बूलियन वापसी करेगा, जो बताएगा कि डाइरेक्ट्री मौजूद है या नहीं। नीचे एक उदाहरण दिया गया है:

```TypeScript
import fs from 'fs';

const directory = '/Users/username/documents';

if (fs.existsSync(directory)) {
  console.log("The directory exists!");
} else {
  console.log("The directory does not exist.");
}
```

आउटपुट:

```TypeScript
The directory exists!
```

इस उदाहरण में, हमने "fs" मॉड्यूल को में "import" किया है और "fs.existsSync()" फंक्शन का उपयोग करके डाइरेक्ट्री की उपस्थिति की जांच की है। हमने इसके बाद संदेश दिया कि डाइरेक्ट्री मौजूद है या नहीं।

## गहन अध्ययन

इस अनुभाग में, हम गहन रूप से देखेंगे कि "fs.existsSync()" फंक्शन कैसे काम करता है। इस फंक्शन में हम पथ को पास करते हैं और फाइल सिस्टम के किसी भी स्तर पर इस पथ की उपस्थिति की जांच की जाती है। यदि डाइरेक