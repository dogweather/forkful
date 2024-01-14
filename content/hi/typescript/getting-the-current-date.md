---
title:                "TypeScript: वर्तमान तिथि प्राप्त करना"
simple_title:         "वर्तमान तिथि प्राप्त करना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## क्यों

JavaScript का एक पॉप्युलर संबंधित फीचर है वर्तमान तिथि को प्राप्त करना। TypeScript में, हमारे पास एक बना पोसिबिलिटी है वर्तमान तिथि को प्राप्त करने का। इस एसे के एकसारंग उदाहरण है कोड स्निपेट के साथ यह शोध करना।

## कैसे करें

वर्तमान तिथि को प्राप्त करने के लिए, हम Date ऑब्जेक्ट का उपयोग कर सकते है। इस ऑब्जेक्ट के कई प्रॉपर्टी होते है। निन्मलिखित कोड स्निपेट में, हम ध्यान दे सकते है की महीना और दिन, दिनांक, समय आदि दिखाते है।

```TypeScript
const today = new Date(); // वर्तमान तिथि को प्राप्त करने के लिए Date ऑब्जेक्ट बनाएं
console.log(today.getMonth()); // महीना का व्युत्पन्न करें (0 से शुरू)
console.log(today.getDate()); // दिन का व्युत्पन्न करें (1 से शुरू)
console.log(today.getFullYear()); // साल का व्युत्पन्न करें
console.log(today.getHours()); // समय का व्युत्पन्न करें (24 घंटे)
console.log(today.getMinutes()); // मिनट का व्युत्पन्न करें
console.log(today.getSeconds()); // सेकंड का व्युत्पन्न करें
```

उपरोक्त कोड स्निपेट का फलना निम्नलिखित हो सकता है:

```bash
5
24
2021
18
30
45
```

## गहराई में जायें

Date ऑब्जेक्ट के साथ, हम भी इस्तेमाल कर सकते है Date.now() फ़ंक्शन जो वर्तमान तिथि को मिलीसेकंड में देती है। आप भी विशिष्ट मिलीसेकंड अनुभागों को प्राप्त कर सकते है और इसे Date ऑब्जेक्ट के साथ मिला सकते है, जैसे निम्नलिखित कोड स्निपेट में दिखाया गया है:

```TypeScript
const now = Date.now(); // वर्तमान तिथि को मिलीसेकंड म