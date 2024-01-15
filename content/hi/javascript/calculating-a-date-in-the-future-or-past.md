---
title:                "भविष्य में या भूतकाल में दिनांक की गणना"
html_title:           "Javascript: भविष्य में या भूतकाल में दिनांक की गणना"
simple_title:         "भविष्य में या भूतकाल में दिनांक की गणना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## लोग क्यों गिनती करें? 
गिनती एक महत्वपूर्ण उपकरण है जिसके उपयोग से हम पिछली और भविष्य की तिथियों को जानते हैं और उसके अनुसार अपनी गतिविधियों को नियोजित कर सकते हैं। इसलिए, आप एक गिनती तिथि का उपयोग करके अपने काम को आसान और सुगम बना सकते हैं।

## कैसे करें गिनती कल्पना
यदि आप अपनी स्क्रिप्टिंग स्किल में सुधार करना चाहते हैं या आप सरल तरीकों से तिथियों को कैलकुलेट करना चाहते हैं तो आपको गिनती कल्पना को सीखना चाहिए। आप निम्नलिखित सामान्य तरीकों का उपयोग करके अपनी गिनती तिथियों को किसी भी भविष्य में कैलकुलेट कर सकते हैं:

```Javascript
// आज की दिनांक उपयोग कर के, 30 दिन परे की तिथि की गिनती करें
let today = new Date();
let futureDate = new Date(today.getTime() + (30 * 24 * 60 * 60 * 1000));
console.log("Future date: ", futureDate);
// Output: Future date: Mon Jun 14 2021 16:51:40 GMT+0530 (India Standard Time)

// आज की दिनांक उपयोग कर के, 15 दिन पहले की तिथि की गिनती करें 
let today = new Date();
let pastDate = new Date(today.getTime() - (15 * 24 * 60 * 60 * 1000));
console.log("Past date: ", pastDate);
// Output: Past date: Mon May 10 2021 15:03:55 GMT+0530 (India Standard Time)

// दी गई तिथि को प्रदर्शित करने के लिए ऑब्जेक्ट बनाएं 
let givenDate = new Date("May 20, 2021");
console.log("Given date: ", givenDate);
// Output: Given date: Thu May 20 2021 00:00:00 GMT+0530 (India Standard Time)
```

## गहराई में जाएँ
गिनती एक उपयोगी तकनीक है जो हमें उपयोगकर्ता के इच्छित तिथि को कैलकुलेट करने में मदद करती है। आप आसान से गिनती तिथियो