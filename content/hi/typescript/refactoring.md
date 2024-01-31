---
title:                "कोड सुधार"
date:                  2024-01-26T03:37:49.968125-07:00
model:                 gpt-4-0125-preview
simple_title:         "कोड सुधार"

category:             "TypeScript"
tag:                  "Good Coding Practices"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/refactoring.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
रिफैक्टरिंग विद्यमान कंप्यूटर कोड को पुनर्गठित करने की प्रक्रिया है बिना इसके बाहरी व्यवहार में परिवर्तन किए। प्रोग्रामर इसे कोड को अधिक साफ, अधिक बनाए रखने योग्य बनाने, और जटिलता को कम करने के लिए करते हैं, जिससे इसे नए सिरे से गोता लगाने वाले किसी के लिए समझना आसान हो जाता है।

## कैसे:
एक TypeScript फ़ंक्शन पर विचार करें जिसने बेहतर दिन देखे हैं - यह थोड़ा गड़बड़ है, और इसे कुछ स्नेह और देखभाल की आवश्यकता हो सकती है:

```typescript
function userInfo(data: any): string {
    return "User Info: " + data.name + ", " + data.age + ", " + data.email + ";" ;
}
```
रिफैक्टर्ड, यह इस तरह दिखाई दे सकता है:

```typescript
interface User {
    name: string;
    age: number;
    email: string;
}

function formatUserInfo(user: User): string {
    return `User Info: ${user.name}, ${user.age}, ${user.email};`;
}
```

दूसरा उदाहरण अधिक मजबूत है, TypeScript की प्रकार प्रणाली का उपयोग करते हुए `interface` के साथ संभावित रनटाइम त्रुटियों से बचने और पठनीयता में सुधार करता है।

## गहराई में
रिफैक्टरिंग एक आधुनिक अवधारणा नहीं है; यह प्रोग्रामिंग के साथ विकसित हुई, 1999 में Martin Fowler की पुस्तक "Refactoring: Improving the Design of Existing Code" के प्रकाशन के साथ अधिक औपचारिक बन गई। यह एक Agile विकास वातावरण में महत्वपूर्ण है, अनुकूली कोड परिवर्तनों को सुविधाजनक बनाते हुए। मैन्युअल रिफैक्टरिंग के विकल्पों में TSLint जैसे स्वचालित उपकरण या TypeScript का अपना भाषा सर्वर शामिल हैं जो कुछ रिफैक्टरिंग कार्यों का सुझाव दे सकते हैं या यहां तक कि उन्हें आपके लिए कर सकते हैं। कार्यान्वयन विवरण आमतौर पर "कोड गंधों" को पहचानने, जैसे डुप्लिकेट कोड, लंबे तरीके, या बड़े वर्ग, और उपचार के लिए पैटर्न लागू करने में शामिल होते हैं-जैसे कि विधियों को निकालना, अधिक उपयुक्त वर्गों में चले जाना, या सरल निर्माणों का उपयोग करना। ये पैटर्न रिफैक्टरिंग के कैसे और क्यों को समझने के लिए महत्वपूर्ण हैं।

## यह भी देखें
- [Martin Fowler द्वारा पुस्तक "Refactoring: Improving the Design of Existing Code"](https://martinfowler.com/books/refactoring.html)
- [स्टैटिक कोड विश्लेषण के लिए TSLint](https://palantir.github.io/tslint/)
- [कोड गंधों को समझना](https://refactoring.guru/refactoring/smells)
