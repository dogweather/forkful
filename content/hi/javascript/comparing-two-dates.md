---
title:                "Javascript: दो तारीखों की तुलना करना"
programming_language: "Javascript"
category:             "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/comparing-two-dates.md"
---

{{< edit_this_page >}}

## क्यों

एक इंसान दो तारीखों को तुलना करने में एंगेज क्यों होगा, यह उसमें से एक है।

## कैसे करें

```Javascript
// एक तारीख से दुसरी तारीख तक का अंतरण निकालें
var date1 = new Date("09/20/2021");
var date2 = new Date("09/25/2021");

var diffInDays = (date2.getTime() - date1.getTime()) / (1000 * 3600 * 24);

console.log(diffInDays);
// आउटपुट: 5
```

```Javascript
// दो तारीखों के बीच कुल सेकंड निकालें
var date1 = new Date("09/20/2021");
var date2 = new Date("09/25/2021");

var diffInSeconds = (date2.getTime() - date1.getTime()) /1000;

console.log(diffInSeconds);
// आउटपुट: 432000
```

```Javascript
// दो तारीखों के बीच कुल मिनट निकालें
var date1 = new Date("09/20/2021");
var date2 = new Date("09/25/2021");

var diffInMinutes = (date2.getTime() - date1.getTime()) / (1000 * 60);

console.log(diffInMinutes);
// आउटपुट: 7200
```

```Javascript
// दो तारीखों के बीच कुल घंटे निकालें
var date1 = new Date("09/20/2021");
var date2 = new Date("09/25/2021");

var diffInHours = (date2.getTime() - date1.getTime()) / (1000 * 3600);

console.log(diffInHours);
// आउटपुट: 120
```

## गहराई में जाइए

दो तारीखों को तुलना करने के लिए, हमें प्रथम तारीख को JavaScript ऑब्जेक्ट में बदलना होगा। साथ ही साथ, हम प्रथम और दूसरी तारीखों के बीच का अंतरण निकालने के लिए getTime() फंकशन का उपयोग कर सकते हैं। इससे हमें दो तारीखों के बीच के मिलनसार अंतर निकालने में आसानी होगी। इसके अलावा, हम इस तरीके को साल, महीने, दिन, घंटे, मिनट और सेकंड के रूप में भी उपयोग कर सकते हैं।

## देखें भी

- [MDN वेब डाक्यूमेंटेशन: तारीख को तुलना करना](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date)
- [JavaScript.info: दो त