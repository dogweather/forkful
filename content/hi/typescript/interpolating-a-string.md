---
title:                "स्ट्रिंग इंटरपोलेशन"
aliases:
- hi/typescript/interpolating-a-string.md
date:                  2024-01-20T17:52:48.541550-07:00
model:                 gpt-4-1106-preview
simple_title:         "स्ट्रिंग इंटरपोलेशन"

tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/interpolating-a-string.md"
---

{{< edit_this_page >}}

## क्या और क्यों?

स्ट्रिंग इंटरपोलेशन एक तरीका है जिसमें हम वैरिएबल या एक्सप्रेशन को सीधे स्ट्रिंग के अंदर डालते हैं। इसका इस्तेमाल कोड में डायनामिक वैल्यूज़ को आसानी से और पढ़ने में आसान तरीके से जोड़ने के लिए किया जाता है।

## कैसे करें:
```typescript
let userName: string = 'अर्जुन';
let age: number = 25;
// टेम्प्लेट लिटरल्स का उपयोग करके स्ट्रिंग इंटरपोलेशन:
let greeting: string = `नमस्ते, मेरा नाम ${userName} है और मैं ${age} साल का हूँ।`;
console.log(greeting);
```
सैंपल आउटपुट:
```
नमस्ते, मेरा नाम अर्जुन है और मैं 25 साल का हूँ।
```

## गहराई से विचार
स्ट्रिंग इंटरपोलेशन का इतिहास जावास्क्रिप्ट के ES6 संस्करण के साथ शुरू हुआ, जहाँ टेम्प्लेट लिटरल्स ने पुराने तरीकों (जैसे कि '+'' ऑपरेटर से स्ट्रिंग्स को जोड़ना) को बदल दिया। टेम्प्लेट लिटरल्स ने कोड को साफ और पढ़ने में आसान बनाया है। TypeScript, जो कि जावास्क्रिप्ट का एक सुपरसेट है, स्वाभाविक रूप से समर्थन करता है स्ट्रिंग इंटरपोलेशन का। विकल्पों की बात करें तो, आप अभी भी पुराने '+' ऑपरेटर का उपयोग कर सकते हैं, पर यह कोड को अधिक क्लटर्ड और त्रुटि-प्रवण बना सकता है। इंटरपोलेशन ज्यादातर टेम्प्लेट लिटरल्स के माध्यम से होता है, जो बैकटिक्स (``) का उपयोग करके बनाए जाते हैं और `${}` का उपयोग करके वैरिएबल या एक्सप्रेशन को शामिल करते हैं। 

## संबंधित स्रोत
- TypeScript हैंडबुक (रस्मी दस्तावेज़): [Template Literal Types](https://www.typescriptlang.org/docs/handbook/2/template-literal-types.html)
- MDN वेब दस्तावेज़: [Template literals](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals) (जावास्क्रिप्ट के लिए)
- ES6 फीचर्स: [Template Literals](http://es6-features.org/#StringInterpolation)
