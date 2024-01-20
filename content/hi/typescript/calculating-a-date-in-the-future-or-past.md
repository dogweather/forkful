---
title:                "भविष्य या अतीत में एक तारीख की गणना"
html_title:           "TypeScript: भविष्य या अतीत में एक तारीख की गणना"
simple_title:         "भविष्य या अतीत में एक तारीख की गणना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

भविष्य या अतीत में तारीख की गणना करना यह समझाना है कि किसी विशेष दिनांक के बाद या उससे पहले कितने दिन होंगे। कार्यक्रमकर्ता इसे इवेंट, मीटिंग, उत्पाद लॉन्च आदि की तारीखों की निगरानी करने के लिए करते हैं।

## कैसे करें:

Date object का उपयोग करके भविष्य या अतीत की तारीख की गणना कर सकते हैं।

```TypeScript
let baseDate = new Date();

let futureDate = new Date();
futureDate.setDate(baseDate.getDate() + 5);

console.log("Future date is: ", futureDate);
```

ऊपरी कोड स्निपेट में, हमने पांच दिनों बाद की तारीख की गणना की है।

## गहराई से जाने:

1. ऐतिहासिक संदर्भ: जावास्क्रिप्ट में तारीख और समय को मैनेज करने के लिए Date object का इस्तेमाल किया जाता है, जो 1970 से उस तारीख और समय तक की मिलीसेकंड की संख्या स्टोर करता है।

2. विकल्प: Moment.js पुस्तक एक शक्तिशाली विकल्प हो सकती है, जिसे तारीखों और समय के साथ काम करने के लिए विकसित किया गया है।

3. कार्यान्वयन विवरण: 'setDate' और 'getDate' मेथड का उपयोग करके हम वर्तमान दिनांक से भविष्य या अतीत की तारीख की गणना कर सकते हैं।

## अन्य स्रोत:

1. [MDN Web Docs: Date - JavaScript](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
2. [Moment.js](https://momentjs.com/)