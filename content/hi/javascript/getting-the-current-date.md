---
title:                "Javascript: वर्तमान तारीख प्राप्त करना"
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## क्यों

वर्तमान दिनांक को प्राप्त करने के कारण, यह आपको अपनी जावास्क्रिप्ट प्रोग्रामिंग में खासी उपयोगी साबित हो सकता है।

## कैसे करें

```Javascript
// वर्तमान दिनांक को प्राप्त करने के लिए जावास्क्रिप्ट का उपयोग करें
let currentDate = new Date();

// दिनांक का प्रारूप ढांचा तय करने के लिए तिथि विधि का उपयोग करें
let day = currentDate.getDate();
let month = currentDate.getMonth() + 1;
let year = currentDate.getFullYear();

// संपूर्ण दिनांक को एक स्ट्रिंग में जोड़ें
let fullDate = day + "/" + month + "/" + year;

// आउटपुट: 23/9/2021
console.log(fullDate);
```

## गहराई में जाएं

जब हम जावास्क्रिप्ट में वर्तमान दिनांक को प्राप्त करते हैं, तो हम वास्तविक दिनांक और समय के साथ साथ उसके अन्य तत्वों को भी प्राप्त करते हैं। हम संभवतः अपने डेट ऑब्जेक्ट की उपयोगिता को बढ़ाने के लिए अन्य तरीकों से उन तत्वों को प्राप्त कर सकते हैं। इससे हमारे पास अधिकतम नियंत्रण और उपयोगिता होती है जो हमें अपने अनुप्रयोगों के साथ काम करने में मदद करती है।

## देखें भी

[जावास्क्रिप्ट तिथि विधियों का उपयोग करना](https://www.w3schools.com/js/js_date_methods.asp)

[वर्तमान दिनांक को प्राप्त करने के लिए जावास्क्रिप्ट का उपयोग कैसे करें](https://stackoverflow.com/questions/1531093/how-do-i-get-the-current-date-in-javascript)

[तिथि और समय को फॉर्मैट करने के लिए जावास्क्रिप्ट तारीख विधियाँ](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)

**यदि आप जावास्क्रिप्ट प्रोग्रामिंग से अधिक स