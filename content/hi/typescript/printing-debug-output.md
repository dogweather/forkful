---
title:                "डीबग आउटपुट प्रिंट करना"
html_title:           "Gleam: डीबग आउटपुट प्रिंट करना"
simple_title:         "डीबग आउटपुट प्रिंट करना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/printing-debug-output.md"
---

{{< edit_this_page >}}

## क्या और क्यों? (What & Why?)

डीबग आउटपुट प्रिंट करना मतलब किसी कोड का रन टाइम परिणाम देखना होता है। निर्माणकर्ता इसका उपयोग त्रुटियों को खोजने और अपने कोड के प्रदर्शन को समझने के लिए करते हैं।

## कैसे (How to)

निम्नलिखित कोड उदाहरण देखें:

```TypeScript
console.log('डीबग मैसेज');
```
आउटपुट:

``` 
डीबग मैसेज 
```

## गहराई में अध्ययन (Deep Dive)

1. **ऐतिहासिक संदर्भ**: `console.log` का उपयोग डीबगिंग के लिए कांसोल पर आउटपुट प्रिंट करने के लिए होता है। यह जावास्क्रिप्ट (और इसके स्थानापन्न TypeScript) में सामान्यतः उपयोग होने वाली तकनीक है।
2. **विकल्प**: कुछ समय में `console.warn` या `console.error` का उपयोग विभिन्न स्तरों की सूचना के लिए किया जाता है।
3. **कार्यान्वयन विवरण**: `console.log` वेब ब्राउज़र या Node.js के कंसोल में संदेश प्रिंट करता है। इसका उपयोग कार्यान्वित कोड का पाठक दृश्य प्रदान करने के लिए किया जाता है।

## अन्य उपयोगी लिंक (See Also)

- जावास्क्रिप्ट और TypeScript डीबगिंग में [console.log(), console.error() और console.warn()](https://developer.mozilla.org/en-US/docs/Web/API/Console) के और उपयोग के बारे में अधिक जानने के लिए।
- डीबगिंग के अधिक उन्नत तरीकों, जैसे की ट्रेस आउटपुट या श्रंखला डंपिंग के बारे में [और अधिक पढ़ें](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/Error/Stack)।