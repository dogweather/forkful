---
title:                "TypeScript: भबिष्य या भूतकाल में एक दिन की गणना"
programming_language: "TypeScript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्यों

कभी-कभी हमें भविष्य या अतीत की तारीख निकालने की जरूरत हो सकती है, जैसे किसी इमारत का निर्माण करने की तारीख, किसी समाचार के प्रकाशन की तारीख आदि। TypeScript प्रोग्रामिंग में हम आसानी से ऐसी तारीखों को निकाल सकते हैं जो भविष्य या अतीत की हो।

## कैसे करें

इस कार्य को करने के लिए हमें `Date` ऑब्जेक्ट का उपयोग करना होगा। निम्नलिखित कोड ब्लॉक में हम देखेंगे कि कैसे हम अपने कोड में `Date` ऑब्जेक्ट को इस्तेमाल कर सकते हैं और कैसे हम भविष्य या अतीत की तारीखों को निकाल सकते हैं।

```TypeScript
// अपने कोड में Date ऑब्जेक्ट को इम्पोर्ट करें
import { Date } from 'typescript';

// अब हम Date ऑब्जेक्ट का उपयोग करके भविष्य की तारीख निकाल सकते हैं
let futureDate = new Date();

// यहां हम भविष्य की तारीख से 10 दिन आगे की तारीख निकाल रहे हैं
futureDate.setDate(futureDate.getDate() + 10);

console.log(futureDate); // Output: 2020-06-22T00:00:00.000Z

// अब आइसीटी स्पैन में हम अतीत की तारीख निकालेंगे
let pastDate = new Date();

// यहां हम अतीत की तारीख से 5 दिन पहले की तारीख निकाल रहे हैं
pastDate.setDate(pastDate.getDate() - 5);

console.log(pastDate); // Output: 2020-06-07T00:00:00.000Z
```

## गहरी खोज

जब हम `Date` ऑब्जेक्ट का उपयोग करते हैं, तो हमें माह, दिन और साल के साथ तारीख सेट करने की अनुमति मिलती है। इसके अलावा, हम `getDate()`, `getMonth()`, `getFullYear()` जैसी फ़ंक्शन का उपयोग करके भी तारीख के प