---
title:                "नियमित अभिव्यक्तियों का उपयोग"
html_title:           "TypeScript: नियमित अभिव्यक्तियों का उपयोग"
simple_title:         "नियमित अभिव्यक्तियों का उपयोग"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/using-regular-expressions.md"
---

{{< edit_this_page >}}

## क्यों

आप गणितीय संकेतकों और पाठकों को खोजने के लिए रेगुलर एक्सप्रेशन से परिचित हो सकते हैं। यह अक्सर विशिष्ट बनाने के लिए टेक्स्ट या संख्याओं को खोजने में मदद करता है।

## कैसे करें

```TypeScript
// उदाहरण 1: एक सभी अंग्रेजी शब्दों की संख्या को संगठित करें
let regex = /[A-z]+/g;
let text = "This article is about regular expressions.";
let result = text.match(regex);
console.log(result); // output: ["This", "article", "is", "about", "regular", "expressions"]

// उदाहरण 2: एक फूल प्रकार को खोजने के लिए रेगुलर एक्सप्रेशन का उपयोग करें
let regex = /rose|jasmine|lily/g;
let bouquet = "I love jasmine and lilies.";
let result = bouquet.search(regex);
console.log(result); // output: 11 (index of "jasmine")
```

## गहराई से समझना

रेगुलर एक्सप्रेशन टेक्स्ट क्षेत्रों में दिए गए पैटर्न की खोज को परिभाषित करने में मदद करता है। आप उनमें मानक एक्सप्रेशन जैसे शब्दों, वर्णों और संख्याओं को भी जोड़ सकते हैं। यह सुनिश्चित करता है कि आप विशेष प्रकार के गैर-अंकितों को नहीं छोड़ने पाते हैं।

## देखें भी

- [टाइपस्क्रिप्ट मैथ ऑब्जेक्ट्स के लिए Regular Expressions](https://www.typescriptlang.org/docs/handbook/declaration-files/do-s-and-don-ts.html#regular-expressions)
- [Javascript में रेगुलर एक्सप्रेशन (MDN)](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Guide/Regular_Expressions)