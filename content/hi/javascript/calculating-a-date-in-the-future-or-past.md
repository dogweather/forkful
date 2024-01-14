---
title:                "Javascript: भविष्य या भूतकाल में एक तारीख का गणना"
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्यों

यदि आपने कभी सेकंड्स, मिनट्स, घंटे, दिन या साल तक गणना है, तो आपको यह पता होगा कि यह गणना थोड़ी चुनौतीपूर्ण हो सकती है। इसलिए, हम कभी-कभी आगे और पीछे दिनांकों की गणना करने के लिए जावास्क्रिप्ट प्रोग्रामिंग का उपयोग करते हैं। इसके अलावा, कुछ ऐसे स्थितियां होती हैं जहां हमें किसी निश्चित दिनांक या समय के लिए गणना करने की आवश्यकता होती है। इस ब्लॉग पोस्ट में, हम आपको बताएंगे कि कैसे जावास्क्रिप्ट का उपयोग करके आप आसानी से आगे और पीछे दिनांकों की गणना कर सकते हैं।

## कैसे करें
गणना की तारीख को प्राप्त करने के लिए, हम कुछ जावास्क्रिप्ट फंक्शन का इस्तेमाल करेंगे - `getDate()`, `getMonth()`, `getFullYear()` और `setDate()`. ये फंक्शन आपको दिनांक के संख्यात्मक मानों को प्राप्त करने और सेट करने में मदद करते हैं। नीचे कुछ कोड उदाहरण दिए गए हैं:

```Javascript
// अगले महीने की तारीख प्राप्त करें
const now = new Date();
const nextMonth = new Date(now.getFullYear(), now.getMonth() + 1, 1);
// output: 'Sat Feb 01 2020 00:00:00 GMT+0530 (India Standard Time)'

// 5 साल बाद की तारीख प्राप्त करें
const now = new Date();
const fiveYearsLater = new Date(now.getFullYear() + 5, now.getMonth(), now.getDate());
// output: 'Mon Feb 26 2024 00:00:00 GMT+0530 (India Standard Time)'

// 10 दिन पहले की तारीख सेट करें
const now = new Date();
now.setDate(now.getDate() - 10);
// output: 'Tue Feb 26 2020 16:47:11 GMT+0530 (India Standard Time)'
```

यहां `getDate()` तारीख को प्राप्त करने के लिए होता है, `getMonth()` महीने को प्राप्त करने के