---
title:                "टेक्स्ट फ़ाइल लिखना"
html_title:           "Gleam: टेक्स्ट फ़ाइल लिखना"
simple_title:         "टेक्स्ट फ़ाइल लिखना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/writing-a-text-file.md"
---

{{< edit_this_page >}}

## Kyun

Aksar humko files mein apne code ko store karne ki zaroorat hoti hai. Lekin kai baar humare paas ye facility nahi hoti ki hum graphical interface mein code ko likh sake. Isi situation mein text files ka use karna bahut helpful ho sakta hai. Is article mein hum baat karenge ki text file kaise likhein.

## Kaise Karein

Pahle ```Gleam.io``` ke official website se ```Gleam``` installer download karein aur apne system mein install kar lein. Aapko ek text editor bhi install karna hoga, jaise ```Notepad``` ya ```Sublime Text```. Ab ek naya file create karein aur usmein ```Gleam``` code likhein, jaise:

```Gleam
pubfn main() {
  let greeting = "Namaste, duniya!";
  io.print(greeting)
}
```

Is code mein humne ```pubfn``` keyword ka use kiya hai, jo ek function declare karta hai. Humne ek variable ```greeting``` bhi define kiya hai aur usmein ek string value assign ki hai. Fir humne ```io.print``` function ko use karke variable ki value print ki hai. Ye ek basic code hai jisse aap text file mein likh sakte hain. Is code ko aap "example.gleam" naam se save kar sakte hain. Ab aap apne terminal ya command prompt mein ```gleam run example.gleam``` command ko use karke apne code ko run kar sakte hain. Iske output ke roop mein aap "Namaste, duniya!" ka message dekhenge.

## Deep Dive

Aap ```Gleam``` ke official documentation ko follow karke text files mein code likhne ke sath sath, file ko compile, run aur build bhi kar sakte hain. Aap ```Gleam``` ke powerful features jaise ki pattern matching, error handling aur type checking ka bhi use kar sakte hain. Agar aapke paas koi bhi doubt ya question hai, to aap ```Gleam``` ke official community forum ya Discord server se madad le sakte hain.

## Dekhen Bhi

- [Gleam.io](https://gleam.io/)
- [Gleam ki Official Documentation](https://gleam.run/getting-started/introduction/)
- [Gleam ki Official Community Forum](https://github.com/gleam-lang/gleam/discussions)
- [Gleam ke Discord Server](https://discord.gg/2rYBnEJgSd)