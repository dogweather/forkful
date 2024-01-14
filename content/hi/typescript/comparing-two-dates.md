---
title:                "TypeScript: दो तारीखों की तुलना करना"
programming_language: "TypeScript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## क्यों
कई प्रोग्रामिंग कार्यों में हमें दो तारीखों को समानता या अंतर को जानने की आवश्यकता होती है। यह जानना आवश्यक है कि कौन सी तारीख बड़ा या छोटा है, ताकि हमें समय और तारीख के साथ संबंधित गणनाओं को सही से कर सकें।

## कैसे करें
दो तारीखों को तुलना करने के लिए, हम तारीख ऑब्जेक्ट का उपयोग कर सकते हैं। नीचे तुलना करने के लिए कुछ कोड दिया गया है:

```TypeScript
const date1: Date = new Date("2021-01-01");
const date2: Date = new Date("2022-01-01");

// दो तारीखों का मिलान या अंतर करने के लिए
const differenceInDays: number = Math.abs(date1.getTime() - date2.getTime()) / (1000 * 60 * 60 * 24);
// दो तारीखों के बीच के दिनों की संख्या मिलेगी

// दो तारीखों का आमंत्रण करने के लिए
if (date1 > date2) {
    console.log("Date 1 is after Date 2");
} else if (date1 < date2) {
    console.log("Date 2 is after Date 1");
} else {
    console.log("Both dates are equal");
}

// दो तारीखों को अलग-अलग ढंग से प्रिंट करने के लिए
console.log(date1.toDateString()); // तारीख को चाहिए पूरी तरह
console.log(date2.toDateString()); // तारीख का साल को छोड़ देना हो
```

आउटपुट:

```
365 // क्योंकि दोनों तारीखों के बीच एक साल होता है

Date 2 is after Date 1

Thu Dec 31 2020
Fri Jan 01
```

## गहराई में जाएं
`Date` ऑब्जेक्ट में कई फ़ंक्शन्स और मेथड्स होते हैं जो तारीखों को तुलना करने के लिए उपयोगी हो सकते हैं। `getMonth()` ऐसा एक मेथड है जो तारीख की महीने को लौटाता है। `setFullYear()` में आप साल को अपडेट कर सकते हैं और `getDate()` तारीख क