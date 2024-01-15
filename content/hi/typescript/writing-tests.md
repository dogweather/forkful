---
title:                "परीक्षाएं लिखना"
html_title:           "TypeScript: परीक्षाएं लिखना"
simple_title:         "परीक्षाएं लिखना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/writing-tests.md"
---

{{< edit_this_page >}}

## क्यों

टाइपस्क्रिप्ट परीक्षण लिखने का कोई न कोई निर्दिष्ट कारण होता है। यह आपको अपने कोड की परफॉर्मेंस बढ़ाने और बग्स को सुलझाने में मदद कर सकता है।

## कैसे करें

```TypeScript
// स्ट्रिंग सपोजिशन परीक्षण
const name = 'आदित्य';
expect(name).toEqual('आदित्य');

// एरे परीक्षण
const numbers = [2, 4, 6];
expect(numbers).toContain(4);
expect(numbers).toHaveLength(3);
```

## गहराई में

परीक्षण लिखना कोड को सुधारने और उसे स्थिर बनाने का बहुत अच्छा तरीका है। यह आपको कोड में बदलाव को कम करने की सहायता करता है और नई फीचर्स जोड़ने से पहले उन्हें सुरक्षित ढंग से जोड़ने में मदद करता है।

## अधिक जानें

- [टाइपस्क्रिप्ट परीक्षण लेख](https://www.typescriptlang.org/docs/handbook/testing.html)
- [Jest टाइपस्क्रिप्ट समर्थन](https://jestjs.io/docs/getting-started#typescript-support)
- [मूल Jest डाको जांच उदाहरण](https://jestjs.io/docs/en/expect)