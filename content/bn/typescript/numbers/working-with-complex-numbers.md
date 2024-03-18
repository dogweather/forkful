---
title:                "জটিল সংখ্যার সাথে কাজ করা"
date:                  2024-03-17T18:39:10.956752-06:00
model:                 gpt-4-0125-preview
changelog:
  - 2024-03-17, gpt-4-0125-preview, translated from English
---

{{< edit_this_page >}}

## কী এবং কেন?
সম্মিশ্র সংখ্যা, যা একটি বাস্তব অংশ এবং একটি কাল্পনিক অংশ (সাধারণত a + bi হিসাবে লিখিত) নিয়ে গঠিত, শুধু বাস্তব সংখ্যা দিয়ে অসাধ্য বা অসম্ভব গণনা সম্ভব করে তোলে। প্রোগ্রামাররা এই সম্মিশ্র সংখ্যার ব্যবহার করে থাকেন সিগনাল প্রক্রিয়াকরণ, কোয়ান্টাম কম্পিউটিং, এবং প্রযুক্ত গণিতের মতো ক্ষেত্রে, যেখানে দ্বি-মাত্রিক সংখ্যা উপস্থাপনা অপরিহার্য।

## কিভাবে:
TypeScript এ সম্মিশ্র সংখ্যা নিয়ে কাজ করা একটি নিবেদিত শ্রেণী চায়। আসুন একটি তৈরি করি এবং যোগ ও গুণের মধ্য দিয়ে কাজ করি।

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

console.log(`Sum: ${sum.toString()}`); // Output: Sum: 4 + 6i
console.log(`Product: ${product.toString()}`); // Output: Product: -5 + 10i
```

## গভীর ডুব
ঐতিহাসিকভাবে, সম্মিশ্র সংখ্যাকে বিতর্কিত বা 'কাল্পনিক' হিসেবে আখ্যা দেওয়া হয়েছিল - প্রাথমিক সন্দেহজনকতাকে প্রকাশ করার জন্য। এখন, তারা আধুনিক গণিত এবং বিজ্ঞানে মৌলিক।

আমাদের সাধারণ শ্রেণীর বিকল্প হতে পারে `math.js` বা `complex.js` মতো বিদ্যমান লাইব্রেরিগুলির ব্যবহার, যা ত্রিকোণমিতি ফাংশন, পূর্ণাঙ্গতা এবং জটিল সহজতা মতো অতিরিক্ত বৈশিষ্ট্যে সমৃদ্ধ।

আমাদের TypeScript বাস্তবায়নের বিস্তারিত অংশ গাণিতিক অপারেশন সংজ্ঞায়িত করা। `add` পদ্ধতি সহজেই সংশ্লিষ্ট যোগ করে। `multiply` গণিতে ব্যবহৃত FOIL পদ্ধতি প্রয়োগ করে, মনে রাখবে যে `i^2 = -1`।

## আরও দেখুন
সম্মিশ্র সংখ্যা এবং প্রোগ্রামিংয়ে তাদের ব্যবহার সম্পর্কিত আরও পড়াশোনা এবং সম্পদের জন্য দেখুন:

- MDN সম্মিশ্র সংখ্যা বীজগণিত: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Global_Objects/BigInt
- `math.js` লাইব্রেরি: https://mathjs.org/docs/datatypes/complex_numbers.html
- `complex.js` লাইব্রেরি: https://complex-js.github.io/complex.js/
