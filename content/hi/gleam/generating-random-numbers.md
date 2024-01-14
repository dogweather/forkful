---
title:    "Gleam: यादृच्छिक संख्याओं को उत्पन्न करना"
keywords: ["Gleam"]
---

{{< edit_this_page >}}

## क्यों

क्या आप कभी सोचा है कि कंप्यूटर कैसे रैंडम नंबर्स बनाता है? रैंडम नंबर्स कैसे आपकी लाइफ को बेहतर बना सकते हैं? अगर हाँ, तो मैं आपको बताना चाहूँगा कि Gleam प्रोग्रामिंग भाषा इस काम के लिए बहुत उपयोगी है। इस ब्लॉग पोस्ट में, हम रैंडम नंबर्स कैसे उत्पन्न करते हैं और इसके क्या फायदे हो सकते हैं, इसके बारे में चर्चा करेंगे।  

## कैसे करें?

```Gleam
import math/random

let num = random.uniform(1, 10)

```

ऊपर दिए गए कोड ब्लॉक में हमने random मॉड्यूल को इंपोर्ट किया है और फिर random.uniform फंक्शन का इस्तेमाल करके दोनों संख्याओं के बीच से एक रैंडम नंबर बनाया है। यही तरीका आपको भी अपने प्रोग्राम में उस कल्पना शीघ्रता जोड़ने का दर्शाता है। 

```Gleam
import math/random

let list = []

for num in range(1, 10) {
  list = list.add(random.uniform(1, 10))
}

```

ऊपर दिए गए कोड ब्लॉक में हमने एक फ़ॉर लुप का उपयोग करके प्रत्येक बार रैंडम नंबर को list लिस्ट में जोड़ा है। यह तरीका आपको रैंडम नंबर्स की सटीकता को बढ़ाने में मदद कर सकता है। 

## गहराई में जाएं

रैंडम नंबर्स ज्यादातर गेम, क्रिप्टोग्राफी, मॉडलिंग और स्टॉक मार्किट की तुलना में भी बहुत उपयोगी होते हैं। Gleam में रैंडम नंबर्स को उत्पन्न करने के लिए मैथ्स मॉड्यूल दो प्रमुख तरीके प्रद