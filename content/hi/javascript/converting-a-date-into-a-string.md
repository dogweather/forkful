---
title:                "Javascript: एक दिनांक को एक स्ट्रिंग में कनवर्ट करना"
simple_title:         "एक दिनांक को एक स्ट्रिंग में कनवर्ट करना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## क्यों

किसी भी विषय पर प्रोग्रामिंग करते समय, समय और तारीख हमेशा अहम् भाग होते हैं। जावास्क्रिप्ट में तारीखों की गणना करने के लिए, हमें तारीख को एक स्ट्रिंग में परिवर्तित करने की जरूरत पड़ती है। इसका उपयोग किसी भी प्रकार की अनुप्रयोगिक तारीख दिखाने में भी किया जा सकता है। इसलिए, अपने जावास्क्रिप्ट प्रोग्रामिंग स्किल्स को बेहतर बनाने के लिए, तारीख को स्ट्रिंग में परिवर्तित करना आवश्यक होता है। 

## कैसे करे

```Javascript
let today = new Date(); // वर्तमान तारीख को पकड़ कर एक नया तारीख ऑब्जेक्ट बनाएं
let dateString = today.toDateString(); // तारीख को एक स्ट्रिंग में परिवर्तित करें
console.log(dateString); // Output: Tue Sep 29 2020
```

इस उदाहरण में, हमने `new Date()` का उपयोग करके वर्तमान तारीख को पकड़ा और फिर `toDateString()` का उपयोग करके इसे एक स्ट्रिंग में परिवर्तित किया। 

## गहराई में जाएं

इस उदाहरण में, हमने स्ट्रिंग को प्रिंट करने के लिए `console.log()` का उपयोग किया है। लेकिन हम उसे अन्य विधियों से भी प्रिंट कर सकते हैं, जैसे कि `alert()` या `document.write()`। इसके अलावा, हम स्ट्रिंग के भीतर किसी भी तारीख वैल्यू को प्राप्त कर सकते हैं और उसे अपनी आवश्यकतानुसार फॉर्मेट कर सकते हैं। 

## और देखें

- [Date Object in Javascript] (https://www.w3schools.com/js/js_dates.asp)
- [Convert Date to String in Javascript] (https://www.w3schools.com/js/js_date_formats.asp)
- [Javascript Date Methods] (https://www.w3schools.com/js/js_date_methods.asp)