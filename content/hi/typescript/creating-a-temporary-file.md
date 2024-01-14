---
title:                "TypeScript: अस्थायी फ़ाइल बनाना"
simple_title:         "अस्थायी फ़ाइल बनाना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/creating-a-temporary-file.md"
---

{{< edit_this_page >}}

## क्यों

क्या आपने कभी सोचा है कि आपने काम करने के लिए किसी अस्थायी फ़ाइल की ज़रूरत क्यों पड़ी है? वह ज़रूरतः आपने एक अस्पष्ट फ़ाइल को स्थानांतरित करना या उसे साफ़ करना था। इस लेख में, हम आपको बताएंगे कि आप कैसे TypeScript का उपयोग करके अस्थाई फ़ाइलें बना सकते हैं जो आपके कोड को साफ़ करने या अस्पष्ट फ़ाइलों को स्थानांतरित करने में मदद कर सकती हैं।

## कैसे करें

अस्थाई फ़ाइलें बनाने के लिए, हम `fs` मॉड्यूल के `tmpdir()` और `tmpNameSync()` फ़ंक्शन का उपयोग कर सकते हैं। कोड को साफ़ करने के लिए, हम `unlinkSync()` फ़ंक्शन का उपयोग कर सकते हैं। नीचे दिए गए कोड ब्लॉक में हम इन फ़ंक्शनों को कैसे उपयोग करते हैं:

```TypeScript
import fs from 'fs';

// Temporary file name
const tmpfilename = fs.tmpNameSync();

// Writing to temporary file
fs.writeFileSync(tmpfilename, 'Hello World!');

// Reading from temporary file
console.log(fs.readFileSync(tmpfilename, 'utf-8'));

// Deleting temporary file
fs.unlinkSync(tmpfilename);
```

यह कोड आपको एक अस्थाई फ़ाइल बनाने, उसमें डेटा लिखने, उससे डेटा पढ़ने और उसे साफ़ करने का तरीका दिखाता है। आप भी इससे थोड़ा बदलाव करके आपनी ज़रूरत के अनुसार कोड को समायोजित कर सकते हैं। इस तरह से, आप अस्थाई फ़ाइलें इज़्ज़त संज्ञा और सुविधा और प्रोग्राम दीये उम्मीदवारों के लिए अपना कोड सुदृढ़ कर सकते हैं।

## गहराई में जाएं

जब हम कोड को स्थानांतरित करते हैं,