---
title:                "Go: स्ट्रिंग जोड़ना"
simple_title:         "स्ट्रिंग जोड़ना"
programming_language: "Go"
category:             "Go"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/concatenating-strings.md"
---

{{< edit_this_page >}}

## Kyun

Strings ko concatenate karne ka kya mahatva hai? Strings ko concatenate karne se aap apne Go programming skills ko improve kar sakte hai, jisse aap apne code ko efficient bana sakte hai aur time aur memory resources ko bacha sakte hai.

## Kaise Karein

Strings ko concatenate karne ke liye sabse pehle "fmt" package ko import karna hoga. Fir aap ```Go
fmt.Println("Hello" + " " + "World")
```
code block mein dekh sakte hai ki kaise hum multiple strings ko ek saath concatenate kar sakte hai. Iske baad aapko output mein "Hello World" dekhne ko milega.

Agar aap more complex strings ko concatenate karna chahte hai, jaise ki variables aur user input, toh aapko "sprintf" function ka istemal karna hoga. Is tarah se aap multiple values ko ek string mein merge kar sakte hai.

## Deep Dive

Concatenating strings ek common task hai aur iska sahi use aapke code ko optimize kar sakta hai. Lekin iske saath saath, strings immutable hote hai, jisse har baar concatenate karne par ek naya string create hota hai. Isse aapke code ka performance thoda slow ho sakta hai. Isliye, agar aapko multiple strings ko concatenate karne ka kaam hai toh "strings.Builder" ka istemal karna chaiye. Isse aap apne code ko efficient bana sakte hai aur memory resources ko bacha sakte hai.

## Dekhiye Bhi

- [Go Language Documentation](https://golang.org/doc/)
- [Strings package in Go](https://golang.org/pkg/strings/)
- [Concatenating Strings in Go - GeeksforGeeks](https://www.geeksforgeeks.org/how-to-concatenate-strings-in-go/)
- [Effective Go: String Append](https://golang.org/doc/effective_go.html#string_appends)