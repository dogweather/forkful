---
title:                "स्ट्रिंग को कैपिटलाइज़ करना"
html_title:           "C: स्ट्रिंग को कैपिटलाइज़ करना"
simple_title:         "स्ट्रिंग को कैपिटलाइज़ करना"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

## What & Why? (क्या और क्यों?)

String capitalization मतलब string के पहले अक्षर को बड़े आकार (uppercase) में बदलना है। Programmers इसे पढ़ने में आसानी के लिए और खास formatting नियमों को पालन करने के लिए करते हैं।

## How to: (कैसे करें:)

```typescript
function capitalizeFirstLetter(str: string): string {
  return str.charAt(0).toUpperCase() + str.slice(1);
}

let greeting = 'namaste world';
console.log(capitalizeFirstLetter(greeting)); // Namaste world
```

Sample Output:

```
Namaste world
```

## Deep Dive (गहरा डाइव)

String capitalization का इस्तेमाल बहुत पहले से होता आ रहा है। Headings, book titles, और sentences की शुरुआत में पहले अक्षर को बड़ा किया जाता है। JavaScript या TypeScript में इसे manually करना पड़ता है, क्योंकि built-in function नहीं है इसके लिए।

दो alternatives हैं `charAt()` के अलावा: string index `[0]` का उपयोग करना, और `substring()` function का इस्तेमाल करना `slice()` के जगह। दोनों ही तरीके सही हैं, पर `charAt()` खाली string पर काम करने में safe है, क्योंकि यह खाली string लौटाएगा जबकि `[0]` undefined लौटाएगा।

## See Also (इसे भी देखें)

- MDN Web Docs [String.prototype.toUpperCase()](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/toUpperCase)
- TypeScript Handbook [Basic Types](https://www.typescriptlang.org/docs/handbook/2/everyday-types.html)
