---
title:    "Arduino: एक अस्थायी फ़ाइल बनाना"
keywords: ["Arduino"]
---

{{< edit_this_page >}}

## क्यों

हाल ही में, तकनीकी उन्नति ने हमारे साथ कई नये समाधान लाये हैं जो हमारे जीवन को आसान बनाने में मदद करते हैं। ऑर्डुनो भी एक ऐसा उपकरण है जो दुनिया भर में लोगों द्वारा प्यार और अपनाया जा रहा है। आज हम जानेंगे कि अगर आप एक ऑर्डुनो प्रोग्रामर हैं तो आप किसी विशेष कारण से कैसे आप एक अस्थायी फाइल बना सकते हैं।

## कैसे करें

आइए जानें कि ऑर्डुनो में कैसे एक अस्थायी फाइल बनाया जा सकता है। प्रथम, हमें `File` लाइब्रेरी को इम्पोर्ट करना होगा। फिर हमें `createTempFile()` का उपयोग करके फाइल बनानी होगी। मान लीजिए, आपके ड्राईव का नाम "drive" है और आप चाहते हैं कि फाइल का नाम "temporary" हो, तो निम्न दिए गए कोड का इस्तेमाल करें:

```
#include <Filesystem.h>

File tempFile = File.createTempFile("drive", "temporary");
```

यहां, आपकी अस्थायी फाइल तैयार है। आप अपने कोड के हिसाब से इसका उपयोग कर सकते हैं। अस्थायी फाइल को हटाने के लिए, आपको `tempFile.delete()` कमांड का उपयोग करना होगा।

## गहराई में जाएं

जब आप ऑर्डुनो में अस्थायी फाइल बनाते हैं, तो आप उस फाइल को सिर्फ तब तक उपयोग कर सकते हैं जब तक कि आप उसे हटा न दें। यह एक अच्छा तरीका है अगर आपको अपने कोड में अस्थायी डेटा की आवश्यकता होती है, लेकिन आप चाहते हैं कि ये डेटा स्थाई होना