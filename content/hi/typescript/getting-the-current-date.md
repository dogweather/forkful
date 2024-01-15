---
title:                "वर्तमान तारीख प्राप्त करना"
html_title:           "TypeScript: वर्तमान तारीख प्राप्त करना"
simple_title:         "वर्तमान तारीख प्राप्त करना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## क्यों

हाल की तारीख प्राप्त करना आपके तारीख से सम्बंधित विभिन्न कामों को काफी आसान बनाता है, जैसे कि इमेज, वीडियो या डॉक्यूमेंट कोड के अंतरिक्ष में पहचान।

## कैसे

यदि आप TypeScript में तारीख को प्राप्त करना चाहते हैं, तो आप Date ऑब्जेक्ट के बारे में जानने की आवश्यकता होगी। नीचे दी गई संग्रह में, मैंने कुछ साधारण तरीके बताए हैं जो आपको मदद करेंगे इस ऑब्जेक्ट की स्थिति तथा प्राप्त की गई तारीख का पता लगाने में।

```typescript
// Τιμή 1
let date = new Date(); //तारीख इन्स्टैंस्टिएट करें

console.log(date); //वर्तमान तारीख दिखाएँ
//उदाहरण आउटपुट: 2021-06-20T17:46:00.023Z

// उदाहरण 2
let date = new Date();
let year = date.getFullYear(); //साल प्राप्त करें
let month = date.getMonth(); //माह प्राप्त करें
let day = date.getDate(); //दिन प्राप्त करें

console.log(`${day}/${month + 1}/${year}`); //अनुक्रम में दिन/माह/साल दिखाएँ
//उदाहरण आउटपुट: 20/6/2021

//उदाहरण 3
let date = new Date();
let hours = date.getHours(); //घंटे प्राप्त करें
let minutes = date.getMinutes(); //मिनट प्राप्त करें
let seconds = date.getSeconds(); //सेकंड प्राप्त करें

console.log(`${hours}:${minutes}:${seconds}`); //समय दिखाएँ
//उदाहरण आउटपुट: 17:56:23
```

## गहराई तक

Date ऑब्जेक्ट की गहराई में जाने के लिए आप प्राप्त की गई तारीख की विभिन्न विश्लेषण भी कर सकते हैं। उदाहरण के लिए, आप उपयोगकर्ता को अंतिम जाँच करने के लिए कितने दिन पहले का चयन करने का विकल्प प्र