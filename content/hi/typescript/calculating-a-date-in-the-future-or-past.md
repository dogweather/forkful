---
title:                "TypeScript: Bhavishya ya Bhoot ki ek tithi ka ganana"
simple_title:         "Bhavishya ya Bhoot ki ek tithi ka ganana"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/calculating-a-date-in-the-future-or-past.md"
---

{{< edit_this_page >}}

## क्यों

अगर आप बड़े प्रोजेक्टस में काम करते हैं जो अलग-अलग देशों के समय जोन में बसा हैं, तो आपको कई बार विभिन्न तारीखों को फिक्स करना पड़ सकता है। इस तरह की स्थिति में आपको आने वाले कुछ सालों या महीनों में एक निश्चित तारीख को निर्धारित करने की जरूरत हो सकती है। ऐसे में, तारीख को फटाफट निकालने के लिए एक अल्गोरिथम का उपयोग किया जा सकता है।

## कैसे करें

कुछ कीबोर्ड्स से आप तारीख, महीना और साल की जानकारी प्राप्त कर सकते हैं। इसके बाद, हमें इस तारीख को निर्धारित संख्या के साथ मिलाना होगा। इसके बाद, हम तारीख को परस्पर सम्बंधित दिन, महीना और साल के साथ जोड़ सकते हैं और इससे नए तारीख को प्राप्त कर सकते हैं। नीचे आप एक यहां कोड से इस अल्गोरिथम का उदाहरण देख सकते हैं।

```TypeScript
function getFutureOrPastDate(date: Date, numberOfYears: number): Date {
    date.setFullYear(date.getFullYear() + numberOfYears);
    return date;
}

const today = new Date();
const futureDate = getFutureOrPastDate(today, 5);

console.log(futureDate.toDateString()); // Output: Sat Sep 18 2026
```

## गहराई में जाएं

इस अल्गोरिथम में हमने `getFutureOrPastDate` नामक एक फ़ंक्शन बनाया है जो स्टार्ट तारीख और संबंधित नंबर ऑफ़ ईयर्स को लेता है और इसे नए तारीख में बदल देता है। हम यहां स्टार्ट तारीख को `Date` टाइप में ले रहे हैं और नए तारीख को भी वापस `Date` टाइप में रिटर्न कर रहे हैं।

इसे आप फिर से कस