---
date: 2024-01-20 17:48:33.976788-07:00
description: "How to: (\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) TypeScript\
  \ me string ki length nikalne ke liye `.length` property ka istemaal hota hai. Simple\
  \ hai, dekho."
lastmod: '2024-04-05T21:53:53.882254-06:00'
model: gpt-4-1106-preview
summary: "(\u0915\u0948\u0938\u0947 \u0915\u0930\u0947\u0902:) TypeScript me string\
  \ ki length nikalne ke liye `.length` property ka istemaal hota hai."
title: "\u0938\u094D\u091F\u094D\u0930\u093F\u0902\u0917 \u0915\u0940 \u0932\u0902\
  \u092C\u093E\u0908 \u091C\u094D\u091E\u093E\u0924 \u0915\u0930\u0928\u093E"
weight: 7
---

## How to: (कैसे करें:)
TypeScript me string ki length nikalne ke liye `.length` property ka istemaal hota hai. Simple hai, dekho:

```TypeScript
let greeting: string = "नमस्ते";

console.log(greeting.length); // Output: 6
```

Ye `6` output dikha raha hai, kyunki "नमस्ते" me 6 characters hain.

## Deep Dive (गहराई में जानकारी)
Historically, JavaScript (aur TypeScript, jo ki JavaScript par based hai) me string ki length property hamesha se rahi hai. Par dhyan rakho, Unicode characters ko count karte waqt, jaise ki emojis ya kuch special symbols, to `.length` thoda differently behave kar sakta hai.

Alternatives ke roop me, libraries jaise ki `lodash` ya `underscore` ko use kar sakte hain, agar aapko advanced string manipulation ki zarurat ho. 

Implementation detail: TypeScript ki `.length` property string ki length ko ek number ke roop me return karti hai. Ye UTF-16 code units ko count karta hai. Iska matlab hai ki agar aapko rare Unicode characters milte hain (jo ki surrogate pairs ka use karte hain), to `.length` accurate na ho.

## See Also (और भी देखें)
- [String.prototype.length - MDN](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/String/length)
- [Understanding JavaScript's `.length` - Stack Overflow](https://stackoverflow.com/questions/543695/what-is-the-length-of-a-string-in-javascript)
- [TypeScript Handbook](https://www.typescriptlang.org/docs/handbook/intro.html)
