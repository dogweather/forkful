---
title:                "उपस्थिति उत्परिवर्तन"
html_title:           "TypeScript: उपस्थिति उत्परिवर्तन"
simple_title:         "उपस्थिति उत्परिवर्तन"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/extracting-substrings.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

प्रोग्रामर्स को कभी-कभी किसी स्ट्रिंग से उसकी छोटी सी भाग को निकालने की जरूरत होती है। यह उन्हें अधिक उचित डेटा प्रवाह हासिल करने और अपने कोड को साफ़ और साफ़ बनाने में मदद करता है।

## कैसे करें:

TypeScript में स्ट्रिंग से उसका छोटा सा भाग निकालने के लिए हम `substring()` फ़ंक्शन का प्रयोग कर सकते हैं। यह फ़ंक्शन दो पैरामीटर लेती है - पहला पैरामीटर स्ट्रिंग की शुरुआती स्थान को और दूसरा पैरामीटर स्ट्रिंग की अंतिम स्थान को देता है। इसका उदाहरण निम्नलिखित है:

```TypeScript
let str: string = "Hello World";
let subStr: string = str.substring(0, 5); // Output: "Hello"
```

## गहराई में जाएं:

`substring()` फ़ंक्शन को 1997 में जावास्क्रिप्ट में प्रदर्शित किया गया था। यह स्ट्रिंग ऑब्जेक्ट के एक प्रवाह के हिस्से को निकासित करता है। इसके अलावा, हम अपने स्ट्रिंग से छोटे से भग को निकालने के लिए `slice()` और `substr()` फ़ंक्शन भी प्रयोग कर सकते हैं। हालांकि, याद रखें कि इन तीनों फ़ंक्शन के मध्य थोड़े-बहुत अंतर हो सकते हैं।

## अन्य जानकारी:

- [ऑफिशियल TypeScript डॉक्यूमेंटेशन](https://www.typescriptlang.org/docs/handbook/release-notes/typescript-1-0.html)
- [जावास्क्रिप्ट में स्ट्रिंग से छोटे भागों को निकालने के लिए अन्य तरीके](https://www.w3schools.com/js/js_string_methods.asp)
- [कोड का उदाहरण](https://stackblitz.com/edit/typescript-substring-example?file=index.ts)