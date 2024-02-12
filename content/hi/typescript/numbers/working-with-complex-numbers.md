---
title:                "जटिल संख्याओं के साथ काम करना"
aliases:
- /hi/typescript/working-with-complex-numbers/
date:                  2024-01-26T04:47:19.930419-07:00
model:                 gpt-4-0125-preview
simple_title:         "जटिल संख्याओं के साथ काम करना"

tag:                  "Numbers"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/working-with-complex-numbers.md"
---

{{< edit_this_page >}}

## क्या और क्यों?
जटिल संख्याएँ, जो एक वास्तविक भाग और एक काल्पनिक भाग से मिलकर बनती हैं (आमतौर पर a + bi के रूप में लिखी जाती हैं), केवल वास्तविक संख्याओं के साथ असंभव या अव्यवहार्य गणनाओं को संभव बनाती हैं। प्रोग्रामर उन्हें सिग्नल प्रोसेसिंग, क्वांटम कम्प्यूटिंग, और लागू गणित जैसे क्षेत्रों में उपयोग करते हैं, जहाँ दो-आयामी संख्या प्रतिनिधित्व महत्वपूर्ण हैं।

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
