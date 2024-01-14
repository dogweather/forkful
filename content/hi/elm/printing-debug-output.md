---
title:                "Elm: डिबग आउटपुट प्रिंट करना"
programming_language: "Elm"
category:             "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/elm/printing-debug-output.md"
---

{{< edit_this_page >}}

## Kyun:

Debug output ko print karne ka matlab hai ki aap apne code ki performance aur errors ko monitor kar sake. Isse aap apne code ke kisi bhi hisse mein agar koi problem ho to usse identify aur solve kar sakte hain. Debug output aapko apne code ko samajhne aur improve karne mein madad karta hai.

## Kaise Kare:

Debug output ko print karne ke liye, aapko Elm ka `Debug.log` function ka istemal karna hoga. Is function mein aapko apne desired message aur value ko pass karna hai jisse aap debug output mein dekhna chahte hain.

```Elm
myNumber = 5
Debug.log "My Number is:" myNumber
```
Ye code debug output mein "My Number is: 5" print karega. Is tarah se aap apne code ki kisi bhi variable ya value ka output dekh sakte hain.

## Gehri Jhaank:

Debug output ka istemal aapko apne code ki performance aur errors ke alawa bhi kuch aur fayde deta hai. Aap isse apne code ki execution flow ko samajh sakte hain aur apne code ko optimize kar sakte hain. Iske alawa, aap apne code mein breakpoints laga kar step through debugging bhi kar sakte hain.

## Dekhe Bhi:

- [Elm ka Debug library](https://package.elm-lang.org/packages/elm/core/latest/Debug)
- [Elm ka Debugging Guide](https://guide.elm-lang.org/debugging/)