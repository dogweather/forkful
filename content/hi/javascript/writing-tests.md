---
title:                "परीक्षण लिखना"
date:                  2024-01-19
simple_title:         "परीक्षण लिखना"

tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/writing-tests.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)
परीक्षण लिखना यानी कोड के छोटे हिस्सों की जाँच करना होता है ताकि ये सुनिश्चित किया जा सके की सब कुछ सही तरीके से काम कर रहा है। प्रोग्रामर्स इसे इसलिए करते हैं क्योंकि इससे बग्स को जल्दी पकड़ा जा सकता है और भविष्य में कोड में परिवर्तन करने के लिए आत्मविश्वास मिलता है।

## How to: (कैसे करें:)
```
// एक साधारण फ़ंक्शन का परीक्षण कैसे करें
function add(a, b) {
    return a + b;
}

// परीक्षण के लिए कोड
function testAdd() {
    const result = add(1, 2);
    if (result !== 3) {
        console.error(`Expected 1 + 2 to equal 3, but got ${result}`);
    } else {
        console.log('Test passed: add(1, 2) === 3');
    }
}

// परीक्षण चलाएं
testAdd();
```
Output:
```
Test passed: add(1, 2) === 3
```

## Deep Dive (गहन अध्ययन):
परीक्षण लिखने की परम्परा बहुत पुरानी है। 'TDD' (Test-Driven Development) जैसी प्रक्रियाएं परीक्षण पर जोर देती हैं और पहले परीक्षण लिखने की सलाह देती हैं। जावास्क्रिप्ट में, 'Jest', 'Mocha', और 'Chai' जैसे टूल्स परीक्षण लिखने के लिए लोकप्रिय हैं। इन टूल्स में 'assertions', 'test runners', और 'mocking' जैसी सुविधाएँ होती हैं जो कोड के परीक्षण को सरल और प्रभावी बनाती हैं।

## See Also (देखें भी):
- Jest: [https://jestjs.io/](https://jestjs.io/)
- Mocha: [https://mochajs.org/](https://mochajs.org/)
- Chai: [https://www.chaijs.com/](https://www.chaijs.com/)
- Test-Driven Development (TDD): [https://en.wikipedia.org/wiki/Test-driven_development](https://en.wikipedia.org/wiki/Test-driven_development)
