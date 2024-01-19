---
title:                "भविष्य या अतीत में एक तारीख की गणना"
html_title:           "Javascript: भविष्य या अतीत में एक तारीख की गणना"
simple_title:         "भविष्य या अतीत में एक तारीख की गणना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

भविष्य या अतीत की तारीख की गणना क्या होती है, आप जैसे शायद समझ रहे होंगे, यह संगणकीय शब्द है जिसका मतलब होता है कि आप एक तारीख के विचारना कर रहे हैं और इसे आगे या पिछे बढ़ा रहे हैं। प्रोग्रामर्स इसे इसलिए करते हैं क्योंकि यह एक आम क्रिया है जो किसी भी प्रोग्राम में उत्प्रेरणात्मकता को बढ़ाता है।

## कैसे करें:

```Javascript
// भविष्य की तारीख की गणना
let futureDate = new Date();
futureDate.setDate(futureDate.getDate() + 10); // 10 दिन बाद की तारीख
console.log(futureDate);

// अतीत की तारीख की गणना
let pastDate = new Date();
pastDate.setDate(pastDate.getDate() - 20); // 20 दिन पहले की तारीख
console.log(pastDate);
```

## गहरी दीक्षा:

तारीखों की गणना का इतिहास काफी पुराना है, जब भी कंप्यूटरों को किसी घटना के आधार पर कार्य करना पड़ता है। JavaScript में `Date` वस्तु तारीखों की गणना को आसान बनाती है, लेकिन यह अधिक सुविधाजनक हो सकता है তਥਾ-third-party libraries, like Moment.js or date-fns का उपयोग करके। उन्होंने विभिन्न समय क्षेत्र, प्रारूप और मानकों के साथ काम करने के लिए API बनाई है । यदि आपको दिनांक में सटीकता की ज़रूरत है, तो `setDate` के बजाय `setUTCHours` का उपयोग करना शायद ठीक रहेगा।

## देखें भी:

- [MDN Web Docs: JavaScript Dates](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Numbers_and_dates#Date_object)
- [Moment.js](http://momentjs.com/)
- [date-fns](https://date-fns.org/)