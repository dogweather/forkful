---
title:    "Javascript: शब्द स्ट्रिंग की लंबाई खोजें"
keywords: ["Javascript"]
---

{{< edit_this_page >}}

## क्यों

क्या आपने कभी सोचा है कि प्रोग्रामिंग में स्ट्रिंग की लंबाई क्यों महत्वपूर्ण होती है? यह स्ट्रिंग ही है जो हमें डेटा को लेन-देन और विभिन्न प्रकार के मानों को लेकर काम करने में मदद करती है। स्ट्रिंग की लंबाई पता करने के माध्यम से हम देख सकते हैं कि कितने वर्ण इसमें हैं और उसको संपादित कर सकते हैं। इसलिए, स्ट्रिंग की लंबाई का ज्ञान होना बहुत जरूरी है।

## कैसे करें

एक स्ट्रिंग की लंबाई निकालने के लिए, हमारे पास कई तरीके हो सकते हैं। यहां, हमारे पास एक सरल और आसान तरीका है जो जावास्क्रिप्ट केवल एक फंक्शन का उपयोग करते हुए देखा जाता है।

```Javascript
function findLength(str) {
  return str.length;
}

console.log(findLength("हिन्दी")); // Output: 5
console.log(findLength("Hello World")); // Output: 11
```

जैसा कि आप देख सकते हैं, हमने स्ट्रिंग की लंबाई निकालने के लिए सरल फंक्शन `length` का उपयोग किया है और उसे अपने स्ट्रिंग के साथ पास किया है। यहां `गुण` के माध्यम से हमें स्ट्रिंग की लंबाई मिल जाती है और हम उसे प्रिंट करते हैं।

## गहराई में जाएं

जब हम `length` फंक्शन का उपयोग स्ट्रिंग की लंबाई निकालने के लिए करते हैं, तो बाजार में कुछ समस्याएं और नियम जोड़े जाते हैं। यह फंक्शन अंतरिक्ष और अन्य विशिष्ट वर्णों को ध्यान में नहीं रखता है। इसलिए, कुछ लंबाई को गिनने और उसे उ