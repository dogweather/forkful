---
title:                "वेब पेज को डाउनलोड करना"
html_title:           "TypeScript: वेब पेज को डाउनलोड करना"
simple_title:         "वेब पेज को डाउनलोड करना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
वेब पृष्ठ डाउनलोड करना क्या है और इसे लेने के लिए प्रोग्रामर क्यों करते हैं?

वेब पृष्ठ डाउनलोड करना एक आम कार्य है जो आप वेब पृष्ठ को एक स्थान से दूसरे स्थान पर प्राप्त करते हैं। प्रोग्रामर इसे करते हैं ताकि आप उन्हें ऑफलाइन मोड में उपलब्ध कर सकें और नेटवर्क कनेक्शन की आवश्यकता न हो।

## कैसे करें?
```TypeScript
// import the 'http' module
import * as http from 'http';

// specify the URL of the web page
const url = 'https://example.com';

// use the 'get' method to request the web page
http.get(url, (res) => {
  // logging the response from the web page
  console.log(`Website downloaded with status code: ${res.statusCode}`);
});
```

Output: `Website downloaded with status code: 200`

## गहराई में जाएं
(1) इतिहासिक परिस्थिति: पहले, प्रोग्रामर वेब पृष्ठों को डाउनलोड करने के लिए केवल सर्धनात्मक तरीके उपलब्ध थे, जो अपेक्षाकृत अस्थायी थे। आज, TypeScript जैसी उच्च-स्तरीय भाषाओं का उपयोग करना वेब पृष्ठ डाउनलोडिंग में आसानी से संभव है। (2) वैकल्पिक: अलग-अलग भाषाओं या उपकरणों का उपयोग करके आप वेब पृष्ठ डाउनलोड को सुधार सकते हैं। (3) अंतर्निहित विवरण: गहराई में जाने के लिए, आप विभिन्न आयामों और HTTP रिक्वेस्ट प्रकारों की जानकारी प्राप्त कर सकते हैं।

## और देखें
सम्बंधित स्रोतों के लिंक:

- [Node.js वेबसाइट डाउनलोड करना](https://nodejs.dev/download-web-page)
- [टाइपस्क्रिप्ट के साथ नेटवर्क रिक्वेस्ट भेजना](https://www.geeksforgeeks.org/send-a-get-request-to-the-server-in-typescript/)
- [वेबसाइट डाउनलोड करना के लिए अन्य तरीके](https://www.techbeamers.com/webscraping-node-scraping-html/)