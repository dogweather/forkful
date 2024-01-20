---
title:                "वर्तमान तारीख प्राप्त करना"
html_title:           "C#: वर्तमान तारीख प्राप्त करना"
simple_title:         "वर्तमान तारीख प्राप्त करना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/getting-the-current-date.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

वर्तमान दिनांक प्राप्त करना मतलब किसी कंप्यूटर की स्थानीय समय को पढ़ने की क्रिया है। निरंतर बदलते हुए डेटा को संभालने, लॉग फ़ाइलों को टाइमस्टैंप करने और डेटा ट्रैक करने के लिए प्रोग्रामर्स इसे करते हैं। 

## कैसे करें:

नीचे दी हुई साधारण जावास्क्रिप्ट कोड की मदद से हम वर्तमान दिनांक प्राप्त करते हैं। यह कोड इसे कंसोल में प्रिंट करेगा। 

```Javascript
let vartmaanDinaank = new Date();
console.log(vartmaanDinaank);
```

जो नतीजा है-

```
2022-04-20T15:23:42.511Z
```

## गहराई में:

जावास्क्रिप्ट में दिनांक और समय को प्राप्त करने के अन्य तरीके भी हैं, जैसे कि `.toDateString()` , `.toGMTString()` आदि। इसका प्रयोग स्पेसिफिक दिनांक स्वरूप के लिए किया जा सकता है।

जावास्क्रिप्ट अपने पहले वर्जन में ही `Date()` ऑब्जेक्ट का समर्थन करता था। हालांकि, इसे समय के साथ विकसित किया गया है और इसमें समय के लिए अधिक क्षमताएं जोड़ी गई हैं।

## और देखें:

1. Mozilla का [जावास्क्रिप्ट दिनांक प्रलेखन](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Date).

2. W3Schools का [जावास्क्रिप्ट दिनांक ट्यूटोरियल](https://www.w3schools.com/js/js_date_methods.asp). 

3. Stack Overflow पर [दिनांक से सम्बंधित प्रश्नोत्तर](https://stackoverflow.com/questions/tagged/javascript+date).