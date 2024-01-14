---
title:    "Ruby: एक पाठ फ़ाइल पढ़ना"
keywords: ["Ruby"]
---

{{< edit_this_page >}}

## क्यों
किसी भी प्रोग्रामर के लिए एक टेक्स्ट फ़ाइल को पढ़ने का काम बहुत महत्वपूर्ण होता है। यह फाइल हमें कोड को समझने और इसे संशोधित करने में मदद करती है। इसलिए, हिंदी पाठकों के लिए आज हम आपको बताएंगे कि टेक्स्ट फ़ाइल को कैसे पढ़ा जाए।

## कैसे करें
एक टेक्स्ट फ़ाइल को रूबी में पढ़ने के लिए, हम डेटा टाइप को स्ट्रिंग में कॉन्वर्ट कर सकते हैं और प्रिंट फ़ंक्शन के साथ इसे प्रिंट कर सकते हैं। हमें फाइल का पूरा पथ निर्दिष्ट करना होगा और फ़लो या ब्लॉक के माध्यम से फ़ाइल को खोलना होगा। कोड ब्लॉक के माध्यम से, हम फ़ाइल को बंद कर सकते हैं।

```Ruby
file = File.open("test.txt", "r")
data = file.read
puts data
file.close
```

इस कोड से हम टेक्स्ट फ़ाइल को पढ़ सकते हैं और प्रिंट कर सकते हैं।

## गहराई में जाएं
फाइल के साथ काम करते समय, हमें ध्यान रखने की आवश्यकता होती है कि हमारी फ़ाइल के डेटा कोलम और रौंदी समान रूप से दिखाई दे। इसलिए, हम टेक्स्ट फ़ाइल के प्रत्येक लाइन को स्पेस के आधार पर स्प्लिट कर सकते हैं। इस तरह, डेटा को स्पेस के आधार पर अलग-अलग डेटा टाइप में अलग किया जा सकता है।

इस तरह जब हम टेक्स्ट फ़ाइल को पढ़ते हैं, हमारे पास पाठ के साथ-साथ डेटा कोलम और रौंदी के लिए अलग अलग चुनाव ह