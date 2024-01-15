---
title:                "एक नियम को मेल खाते हुए अक्षरों को हटाना"
html_title:           "TypeScript: एक नियम को मेल खाते हुए अक्षरों को हटाना"
simple_title:         "एक नियम को मेल खाते हुए अक्षरों को हटाना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## बगाईए क्यों 

किसी के लिए विशेष क्यों चिंता में शामिल होना चाहिए कि उनका जोड़ा परिवर्तितों के आधार पर बगाई जाएगी।

## कैसे करें

```Typescript
let string = "Hello World!";
string = string.replace(/l+/g, "");
console.log(string); // Output: Heo Word!
```

```Typescript
let string = "ABC123def456GHI789";
string = string.replace(/[0-9]+/g, "");
console.log(string); // Output: ABCdefGHI
```

## गहराई में प्रवेश करें

जब हम एक स्ट्रिंग से ओर अधिक विवरण होता है, तो हम एक पैटर्न के आधार पर कैरेक्टर्स को डिलीट कर सकते हैं। इसके अलावा, हम अपने दिए गए पैटर्न के आधार पर स्ट्रिंग को भी विभाजित कर सकते हैं ताकि हम अलग-अलग सेगमेंट में अलग-अलग ऑपरेशन कर सके। यह मजेदार और उपयोगी तकनीक है जो हमें अपने कोड को दुरुस्त बनाने में मदद कर सकती है।

## देखें भी

- [RegExp ओपरेशन की पूर्ण गाइड](https://www.typescriptlang.org/docs/handbook/regular-expression.html)
- [पैटर्न मैचिंग के बारे में अधिक जानकारी](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)