---
title:                "कम्प्यूटर प्रोग्रामिंग पर एक लेख का शीर्षक: कमांड लाइन आर्ग्यूमेंट पढ़ना"
html_title:           "Javascript: कम्प्यूटर प्रोग्रामिंग पर एक लेख का शीर्षक: कमांड लाइन आर्ग्यूमेंट पढ़ना"
simple_title:         "कम्प्यूटर प्रोग्रामिंग पर एक लेख का शीर्षक: कमांड लाइन आर्ग्यूमेंट पढ़ना"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/javascript/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## Kyun
Agar aap ek Javascript developer hain aur command line arguments ke bare mein nahi jaante hain, toh aap koi kaam hone se pehle upar se hi script likhna ya keval debugging ke liye command line arguments ka istemaal karte hain. Par agar aap command line arguments ko jaante hain aur unhe sahi tarah se padh paate hain, toh aapki coding skills aur productivity dono mein improvement hoga. Iss article mein hum aapko Javascript mein command line arguments ko padhna sikhayenge aur aapko iska kya fayda ho sakta hai.

## Kaise Krein
Command line arguments padhna bahut hi aasaan hai Javascript mein. Sabse pehle, aapko `process.argv` object ka use karna hoga. Iss object mein humare program ke command line arguments stored hote hain. Yahaan jo `process` hai wo global object hai jo ki node.js mein available hota hai aur `argv` humare arguments ka array hai. Hum iska use `for` loop ya `forEach` method ki madad se kar sakte hain.

```Javascript
// Example:
for (var i = 0; i < process.argv.length; i++) {
  console.log(`Argument ${i}: ${process.argv[i]}`);
}

// Output:
Argument 0: node
Argument 1: example.js
Argument 2: hello
Argument 3: world
```

Aap dekh sakte hain ki `process.argv` ka pehla element hamesha `node` hota hai, dusra element humare code file ka naam hota hai aur baki ke elements humare command line arguments hote hain. Hum `slice()` method ka bhi use karke `process.argv` mein se unhe extract kar sakte hain.

```Javascript
// Example:
var arguments = process.argv.slice(2); // Ye pehle do elements ko chhodkar array ke baaki elements ko humare arguments mein store karta hai

// Output:
Arguments: ['hello', 'world']
```

## Deep Dive
`process.argv` object ko padhna ek acchi practice hai, lekin aapko pata hona chahiye ki ye sirf command line arguments ko read kar sakta hai jo aapne program ke saath saath pass kiye hain. Agar aapko kisi user se input lena hai, toh aapko `process.argv` ka use nahi karna chahiye. Iske liye aapko `process.stdin` object ka use karna hoga. Aur agar aap user ko specific type ka input lena chahte hain, jaise number ya character, toh aapko usse pehle `parseInt()` ya `parseFloat()` ka use karna hoga.

```Javascript
// Example:
process.stdin.resume(); // Input lene ke liye ye function call karna zaruri hai
process.stdin.setEncoding('utf8'); // Agar aap character input lena chahte hain, toh ye line likhna zaruri hai

process.stdin.on('data', function (chunk) { // Jab stdin se data aayega toh ye function call hoga
  console.log(`Input: ${chunk}`); // chunk variable mein input string stored hoga
});
```

Aap iss tarah se `process.stdin` ka use karke bhi user se input le sakte hain. Agar aapko iss baare mein aur jaankari chahiye, toh aap [Node.js official dokumentation](https://nodejs.org/api/process.html#process_process_stdin) padh sakte hain.

## Dekhen Bhi
- [Node.js official dokumentation](https://nodejs.org/api/process.html#process_process_stdin)
- [Command line arguments in Javascript: process.argv](https://www.tutsmake.com/javascript-command-line-arguments/)
- [Command line arguments in Node.js](https://www.geeksforgeeks.org/node-js-process-argv-property/)