---
title:    "Bash: स्टैंडर्ड त्रुटि पर लिखना"
keywords: ["Bash"]
---

{{< edit_this_page >}}

## क्यों

बश प्रोग्रामिंग आपके लिए एक शक्तिशाली और उपयोगी उपकरण है। यह आपको सिस्टम कमांड लाइन से आपके सिस्टम के साथ संवाद करने की अनुमति देता है और आपको केवल हाथों से एक क्मांड को टाइप करके आपकी सिस्टम को कंट्रोल करने का मौका देता है। लेकिन कभी-कभी हमारे द्वारा लिखी हुई कोड में गलतियां हो सकती है जो हमारे प्रोग्राम को बंद कर सकती है। इससे बचने के लिए हमें विकल्प अपनाने की आवश्यकता होती है जैसे कि डायरेक्ट्री से बाहरियों के साथ काम करना और समस्याओं को सुलझाना। ऐसी स्थिति में, आपको अपने स्क्रिप्ट के दौरान सामान्य बग्स और गलतियों को खोजने के लिए विवेकपूर्वक संवेदनशीलता को जाँचने की आवश्यकता होती है। इसलिए, हमें अपने बश स्क्रिप्ट के एरर या लॉग फ़ाइल में लिखने की आवश्यकता हो सकती है ताकि हम गलतियों को पकड़ सकें तथा कोड की समस्याओं को सुलझा सकें।

## कैसे करें

अपने बश स्क्रिप्ट में सामान्य एरर या गलतियों को पकड़ने के लिए हम `>&2` को उपयोग करते हैं। इससे आपके एरर और स्टैंडर्ड एरर दोनों को आपस में अलग-अलग रखा जाएगा। हम यहां कुछ उदाहरण दे रहे हैं जो आपको इस टेक्निक को बेहद सरल ढंग से समझाएंगे।

```bash
#!/bin/bash
echo "आपका स्क्रिप्ट प्रोसेस हो रहा ह