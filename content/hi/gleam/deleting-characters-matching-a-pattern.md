---
title:                "पैटर्न से मिलते हुए अक्षरों को हटाना"
html_title:           "Gleam: पैटर्न से मिलते हुए अक्षरों को हटाना"
simple_title:         "पैटर्न से मिलते हुए अक्षरों को हटाना"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/deleting-characters-matching-a-pattern.md"
---

{{< edit_this_page >}}

## Kyon

Kayi baar, humein kisi bhi likhit kaam mein se kuch unnischiya alfaz ya characters ko hatana padta hai, jaise ki spam emails mein "[Advertisement]" ya "[SPAM]" likhna. Iske alawa, WhatsApp par naye messages ka notification delete karne ya kisi pattern ke hisab se kisi specific type ke messages ko delete karne ke liye bhi humein characters ko pattern ke saath match karna padta hai. Isse hum apni coding tasks ko automate kar sakte hain aur samay bacha sakte hain. Isiliye, aaj hum dekhenge ki kaise Gleam programming language se characters ko pattern ke saath match karte huye delete kiya ja sakta hai.

## Kaise Karein?

```
Gleam "Characters delete karne ka example"
fn main() {
	let input = "Hum[SPAM]es"[SPAM] }
	println!(input);
}

Output: Hum es
```

Is coding example mein humne "[SPAM]" pattern ke saath match karne ke liye code likha hai, jisse "[SPAM]" ke pehle aur baad ke characters hat jayenge aur humein sirf "Hum es" output milega.

## Deep Dive

Gleam programming language patterns ko bahut acchi tarah handle karta hai. Ismein aap partial matching kar sakte hain, jismein sirf pattern ke matching characters ko delete kiya ja sakta hai aur baki characters ko code mein use kar sakte hain. Iske alawa, wildcards ka bhi use kiya ja sakta hai. Wildcards kisi bhi character or set of characters ke saath match ho sakte hain aur uske jagah hamein kuch bhi output print kar sakte hain.

## See Also

- [Gleam Programming Language website](https://gleam.run/)
- [Gleam Tutorial for Beginners](https://gleam.run/documentation/#tutorial)