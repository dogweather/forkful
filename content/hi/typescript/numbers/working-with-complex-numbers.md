---
date: 2024-01-26 04:47:19.930419-07:00
description: "\u0915\u0948\u0938\u0947: TypeScript \u092E\u0947\u0902 \u091C\u091F\
  \u093F\u0932 \u0938\u0902\u0916\u094D\u092F\u093E\u0913\u0902 \u0915\u094B \u0938\
  \u0902\u092D\u093E\u0932\u0928\u0947 \u0915\u0947 \u0932\u093F\u090F \u090F\u0915\
  \ \u0938\u092E\u0930\u094D\u092A\u093F\u0924 \u0915\u094D\u0932\u093E\u0938 \u0915\
  \u0940 \u0906\u0935\u0936\u094D\u092F\u0915\u0924\u093E \u0939\u094B\u0924\u0940\
  \ \u0939\u0948\u0964 \u0906\u0907\u090F \u090F\u0915 \u092C\u0928\u093E\u0924\u0947\
  \ \u0939\u0948\u0902 \u0914\u0930 \u091C\u094B\u0921\u093C \u0924\u0925\u093E \u0917\
  \u0941\u0923\u093E \u0915\u0947 \u092E\u093E\u0927\u094D\u092F\u092E \u0938\u0947\
  \ \u0915\u093E\u092E \u0915\u0930\u0924\u0947 \u0939\u0948\u0902\u0964."
lastmod: '2024-03-13T22:44:51.878280-06:00'
model: gpt-4-0125-preview
summary: "TypeScript \u092E\u0947\u0902 \u091C\u091F\u093F\u0932 \u0938\u0902\u0916\
  \u094D\u092F\u093E\u0913\u0902 \u0915\u094B \u0938\u0902\u092D\u093E\u0932\u0928\
  \u0947 \u0915\u0947 \u0932\u093F\u090F \u090F\u0915 \u0938\u092E\u0930\u094D\u092A\
  \u093F\u0924 \u0915\u094D\u0932\u093E\u0938 \u0915\u0940 \u0906\u0935\u0936\u094D\
  \u092F\u0915\u0924\u093E \u0939\u094B\u0924\u0940 \u0939\u0948\u0964 \u0906\u0907\
  \u090F \u090F\u0915 \u092C\u0928\u093E\u0924\u0947 \u0939\u0948\u0902 \u0914\u0930\
  \ \u091C\u094B\u0921\u093C \u0924\u0925\u093E \u0917\u0941\u0923\u093E \u0915\u0947\
  \ \u092E\u093E\u0927\u094D\u092F\u092E \u0938\u0947 \u0915\u093E\u092E \u0915\u0930\
  \u0924\u0947 \u0939\u0948\u0902\u0964."
title: "\u091C\u091F\u093F\u0932 \u0938\u0902\u0916\u094D\u092F\u093E\u0913\u0902\
  \ \u0915\u0947 \u0938\u093E\u0925 \u0915\u093E\u092E \u0915\u0930\u0928\u093E"
weight: 14
---

## कैसे:
TypeScript में जटिल संख्याओं को संभालने के लिए एक समर्पित क्लास की आवश्यकता होती है। आइए एक बनाते हैं और जोड़ तथा गुणा के माध्यम से काम करते हैं।

```TypeScript
class Complex {
    constructor(public re: number, public im: number) {}

    add(other: Complex): Complex {
        return new Complex(this.re + other.re, this.im + other.im);
    }

    multiply(other: Complex): Complex {
        return new Complex(
            this.re * other.re - this.im * other.im,
            this.re * other.im + this.im * other.re
        );
    }

    toString(): string {
        return `${this.re} + ${this.im}i`;
    }
}

let num1 = new Complex(1, 2);
let num2 = new Complex(3, 4);
let sum = num1.add(num2);
let product = num1.multiply(num2);

console.log(`Sum: ${sum.toString()}`); // परिणाम: Sum: 4 + 6i
console.log(`Product: ${product.toString()}`); // परिणाम: Product: -5 + 10i
```

## गहराई में जानकारी
ऐतिहासिक रूप से, जटिल संख्याएं विवादास्पद थीं - यहां तक कि उन्हें शुरुआती संदेह व्यक्त करने के लिए 'काल्पनिक' भी कहा गया। अब, वे आधुनिक गणित और विज्ञान में आधारभूत हैं।

हमारी सरल क्लास के विकल्पों में `math.js` या `complex.js` जैसी मौजूदा लाइब्रेरीज का उपयोग शामिल हो सकता है, जिसमें ट्रिगोनोमेट्रिक फंक्शन्स, घातांकन, और जटिल संयोजन जैसी अतिरिक्त विशेषताएं विस्तृत हैं।

हमारे TypeScript कार्यान्वयन विवरण अंकगणितीय ऑपरेशनों को परिभाषित करने तक सीमित हैं। `add` मेथड संबंधित भागों को बस जोड़ता है। `multiply` गणित में उपयोग किए जाने वाले FOIL मेथड को लागू करता है, याद रखते हुए कि `i^2 = -1` है।

## और देखें
प्रोग्रामिंग में जटिल संख्याओं और उनके उपयोग पर आगे पढ़ने और संसाधनों के लिए, देखें:

- MDN जटिल संख्या बीजगणित: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt
- `math.js` लाइब्रेरी: https://mathjs.org/docs/datatypes/complex_numbers.html
- `complex.js` लाइब्रेरी: https://complex-js.github.io/complex.js/
