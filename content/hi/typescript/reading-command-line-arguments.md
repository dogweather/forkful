---
title:                "कम्प्यूटर प्रोग्रामिंग पर एक लेख: कमांड लाइन आर्गुमेंट्स पढ़ना।"
html_title:           "TypeScript: कम्प्यूटर प्रोग्रामिंग पर एक लेख: कमांड लाइन आर्गुमेंट्स पढ़ना।"
simple_title:         "कम्प्यूटर प्रोग्रामिंग पर एक लेख: कमांड लाइन आर्गुमेंट्स पढ़ना।"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/typescript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Kyun

Agar aap TypeScript ka estemaal karte hai, to aapko jaanna zaroori hai ki command line arguments kya hai aur inka istemaal kaise karte hai. Is article mein hum aapko batayenge ki TypeScript mein command line arguments ka istemaal kaise kiya jaata hai.

## Kaise Karein

Command line arguments ko TypeScript mein read karne ke liye, hum `process.argv` ka istemaal karte hai. Iska format hai `process.argv[index]` jahan index 0 se shuru hota hai aur pehla argument ko index 2 par store kiya jaata hai. Is tarah se hum saare arguments ko access kar sakte hai. Neeche ek coding example diya gaya hai jis mein hum ek file ko run karne ke liye require kiye gaye arguments ko print karenge:

```TypeScript
// File Name: arguments.ts

console.log("Chal raha hai!");

console.log("Command line arguments:");

for (let i = 2; i < process.argv.length; i++) {
  console.log(`Index ${i}: ${process.argv[i]}`);
}
```

Output:

```
Chal raha hai!
Command line arguments:
Index 2: abc
Index 3: def
Index 4: ghi
```

## Deep Dive

Command line arguments ko read karne se pehle, humein `string[]` type ka interface define karna hoga. Yeh interface humein `process.argv` ki help se provide kiya jaata hai. TypeScript ke saare standard libraries mein `process.argv` defined hai. 

Agar aap multiple values ko alag alag arguments ke roop mein pass karna chahte hai, to aap backticks (`) ka istemaal kar sakte hai. Is tarah se aap ek string ko multiple arguments ke roop mein pass kar sakte hai.

Interfaces ko define karne ke liye, aap `*.d.ts` files ka estemaal kar sakte hai. Yeh files TypeScript ke type checking ka kaam karte hai aur humare local `typings` folder mein store kiye jaate hai.

## Dekhiye Bhi

- [Official TypeScript Documentation on Command Line Arguments](https://www.typescriptlang.org/docs/handbook/modules.html)
- [TypeScript Tutorials for Beginners](https://www.tutorialspoint.com/typescript/)