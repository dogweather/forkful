---
title:    "Fish Shell: भविष्य या भूतकाल में एक तारीख का गणना करना"
keywords: ["Fish Shell"]
---

{{< edit_this_page >}}

## क्यों
कोई भी मानव कभी-न-कभी भविष्य या भूतकाल की कोई तारीख को जानने का इच्छुक होता है। यह तारीख उनके जन्मदिन, शादी का दिन, अपने प्रिय साथी के जन्मदिन या कोई खास अवसर हो सकता है। हालांकि, हमारे दैनिक जीवन में शायद हम समय से पीछे हट जाते हैं और याद नहीं रख पाते हैं कि कब कोई गुणमान्य तारीख होनी थी। इस समस्या को हल करने के लिए हम फिश शैल का उपयोग कर सकते हैं और इससे हम अपनी हमेशा के लिए खोयी हुई तारीख को दोबारा पाने में सफल हो सकते हैं।

## कैसे करें
फिश शैल में, हम `date` कमांड का उपयोग करके किसी भी तारीख को गणना कर सकते हैं। हम इसके लिए पुरानी या नई तारीख और उसके साथ संबंधित संख्या को इस्तेमाल कर सकते हैं। इसके अलावा, हम अन्य ऑप्शन जैसे कि तारीख के फॉर्मेट, समय ज़ोन, आदि भी दे सकते हैं। नीचे एक कोड ब्लॉक में दिए गए उदाहरण को देखें।

```Fish Shell
# आज से दस दिन बाद की तारीख की गणना करें
date -d 'now + 10 days'
```

यह उदाहरण हमें आज के दिन से पुंजीवादी दिनांक तक की तारीख देगा। हम इस कमांड का उपयोग करके किसी भी तारीख की गणना कर सकते हैं।

```Fish Shell
# पुरानी तारीख के साथ संबंधित संख्या के साथ तारीख की गणना करें
date -d '- 3 months'
```

यह उदाहरण हमें