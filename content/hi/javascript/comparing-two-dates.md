---
title:                "दो तारीखों की तुलना"
html_title:           "Elixir: दो तारीखों की तुलना"
simple_title:         "दो तारीखों की तुलना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

तारीख की तुलना करना, या "Comparing two dates", दो तारीखों के बीच में अंतर का पता लगाना होता है| प्रोग्रामर्स इसे जब करते है कहीं एक तारीख दूसरी तारीख से पहले है या बाद में, जैसे कई कार्यक्रमों की जरूरत होती है|

## कैसे:

निम्नलिखित कोड मिसाल हैं किस प्रकार दो तारीखों को तुलना की जाती है:

```Javascript
let date1 = new Date('2021-12-01');
let date2 = new Date('2022-01-01');

if(date1 > date2) {
  console.log('date1 is later');
} else if(date1 < date2) {
  console.log('date2 is later');
} else {
  console.log('both dates are equal');
}
```
ऊपरी कोड का आउटपुट(`output`) अपेक्षित रूप से "date2 is later" होगा|

## गहरी जांच:

पिछले संस्करणों में, एक जावा स्क्रिप्ट डेवलपर को बहुत जटिलता से गुजरना पड़ता था तारीखों की तुलना के लिए| लेकिन, मॉडर्न जावास्क्रिप्ट में आप ```Date``` ऑब्जेक्ट का उपयोग कर सकते हैं, जो आपको इसे आसानी से करने देती है|

वैकल्पिक रूप से, आप Moment.js, Day.js, जैसे लाइब्रेरी का भी उपऔग कर सकते हैं, उनका उपयोग और ज्यादा क्विक और सेफ तरीका से तारीख की तुलना करने के लिए किया जा सकता है|

## और देखें:

1. [MDN Web Docs: Date Object](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
2. [Moment.js](https://momentjs.com/)
3. [Day.js](https://day.js.org/)