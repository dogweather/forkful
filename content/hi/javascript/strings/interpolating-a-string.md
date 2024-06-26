---
date: 2024-01-20 17:51:44.974787-07:00
description: "How to (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902): String interpolation\
  \ \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F, JavaScript \u092E\u0947\
  \u0902 template literals \u0915\u093E use \u0915\u0930\u0947\u0902. \u092F\u0947\
  \ backticks (`` ` ``) \u0915\u0947 \u0938\u093E\u0925 \u0932\u093F\u0916\u0947 \u091C\
  \u093E\u0924\u0947 \u0939\u0948\u0902 \u0914\u0930 `${}`\u2026"
lastmod: '2024-03-13T22:44:52.966561-06:00'
model: gpt-4-1106-preview
summary: "String interpolation \u0915\u0930\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F\
  , JavaScript \u092E\u0947\u0902 template literals \u0915\u093E use \u0915\u0930\u0947\
  \u0902."
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0907\u0902\u091F\u0930\u092A\
  \u094B\u0932\u0947\u0936\u0928"
weight: 8
---

## How to (कैसे करें):
String interpolation करने के लिए, JavaScript में template literals का use करें. ये backticks (`` ` ``) के साथ लिखे जाते हैं और `${}` के अंदर expressions रखे जाते हैं:

```javascript
let name = 'रोहित';
let greeting = `नमस्ते, ${name}!`;
console.log(greeting); // नमस्ते, रोहित!
```

एक complex example:

```javascript
let user = {
  first: 'राहुल',
  last: 'कुमार'
};
let items = 3;
let total = 75;
let message = `${user.first} ${user.last}, आपकी कुल आइटम्स हैं ${items} और बिल ${total} रुपए है.`;
console.log(message); // राहुल कुमार, आपकी कुल आइटम्स हैं 3 और बिल 75 रुपए है.
```

## Deep Dive (गहराई से जानकारी):
String interpolation का concept पुराने programming languages से शुरू हुआ. इससे पहले, strings को concatenate करने के लिए `+` operator का use होता था, जो की clumsy और error-prone था:

```javascript
let oldWay = 'नमस्ते, ' + name + '!';
```

Template literals का introduction ES6 (ECMAScript 2015) में हुआ, जिसने इसे आसान और clear बना दिया. Alternatives जैसे string formatting libraries (sprintf.js) भी exist करते हैं, लेकिन modern JavaScript में इनकी कम ही जरूरत होती है.

String interpolation में complex expressions और functions भी शामिल किए जा सकते हैं:

```javascript
let price = 499.99;
let taxRate = 0.18;
let finalPrice = `कुल कीमत (सहित कर): ₹${(price * (1 + taxRate)).toFixed(2)}`;
console.log(finalPrice); // कुल कीमत (सहित कर): ₹589.99
```

यहां, `toFixed(2)` function का use करके numbers को two decimal places में format किया गया है और string में directly डाल दिया गया है.

## See Also (और जानकारी के लिए):
- MDN Web Docs on Template Literals: [Template literals (Template strings)](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Template_literals)
- String interpolation in ES6: [Exploring ES6](http://exploringjs.com/es6/ch_template-literals.html)
- JavaScript string handling: [String](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String)
