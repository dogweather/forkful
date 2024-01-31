---
title:                "दो तारीखों की तुलना"
date:                  2024-01-20T17:34:38.165081-07:00
model:                 gpt-4-1106-preview
simple_title:         "दो तारीखों की तुलना"

category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
दो तारीखों की तुलना करना मतलब उनके बीच का अंतर पता लगाना। प्रोग्रामर इसे डेटा साइंस, इवेंट मैनेजमेंट और शेड्यूलिंग में जरूरत पड़ने पर करते हैं।

## How to: (कैसे करें)
```TypeScript
// आज की तारीख
let today: Date = new Date();

// एक स्पेसिफिक तारीख
let specificDate: Date = new Date('2023-03-15');

// तारीखों की तुलना
if(today > specificDate) {
    console.log('आज की तारीख स्पेसिफिक तारीख से नई है।');
} else if(today < specificDate) {
    console.log('आज की तारीख स्पेसिफिक तारीख से पुरानी है।');
} else {
    console.log('दोनों तारीखें समान हैं।');
}
```
सैंपल आउटपुट: `आज की तारीख स्पेसिफिक तारीख से नई है।`

## Deep Dive (गहराई से जानकारी)
तारीखों की तुलना में जावास्क्रिप्ट और टाइपस्क्रिप्ट अंडरलाइंग `Date` ऑब्जेक्ट के मिलीसेकेंड वैल्यू का इस्तेमाल करते हैं। इतिहास में पीछे जाएं, तो ECMAScript स्पेसिफिकेशन पर आधारित जावास्क्रिप्ट प्लैटफॉर्म पर तारीखों का हैंडलिंग थोड़ा कठिन था। लेकिन `Date` क्लास के आने से सब आसान हो गया। विकल्पों की बात करें तो, Moment.js जैसे लाइब्रेरी भी उपलब्ध हैं, लेकिन उनकी ज़रूरत जटिल ऑपरेशंस के लिए होती है। `Date` ऑब्जेक्ट्स की तुलना सीधी है और इसमें समय ज़ोन का प्रबंधन भी शामिल है।

## See Also (और जानकारी के लिए)
- MDN Web Docs पर [Date reference](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date).
- [Moment.js](https://momentjs.com/) - तारीखों को हैंडल करने के लिए एक लोकप्रिय लाइब्रेरी।
- TypeScript की [Handbook](https://www.typescriptlang.org/docs/handbook/intro.html) - TypeScript सीखने के लिए स्टार्टिंग पॉइंट।
