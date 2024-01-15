---
title:                "डायरेक्टरी मौजूद है या नहीं जांचें"
html_title:           "Gleam: डायरेक्टरी मौजूद है या नहीं जांचें"
simple_title:         "डायरेक्टरी मौजूद है या नहीं जांचें"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/gleam/checking-if-a-directory-exists.md"
---

{{< edit_this_page >}}

## Kyun

Agar aap apane computer mein kisi project ko chalane se pahle, uske liye zaroori directories maujood hain ya nahi, ye pata lagana chahate hain, to aap ```Gleam``` programming language ka upyog karke aasani se check kar sakte hain. Isse aap apne project ko kisi bhi error se bacha sakte hain aur sahi directories ko use kar sakte hain. 

## Kaise Karein

Aapko bas kuch simple steps follow karne hain: 

1. Sabse pehle, ```Gleam``` programming language mein ```dir.exists``` function ka use karein.
2. Is function mein, aapko directory ka naam aur path provide karna hoga.
3. Agar directory maujood hai, to function true return karega, warna false.
4. Is tarah, aap easily check kar sakte hain ki kya aapke computer mein desired directory maujood hai ya nahi.

```Gleam
dir.exists("project/directory")
```

Output:
```
true
```

## Gehri Jhaank

Agar aap janna chahate hain ki ```dir.exists``` function kaise kaam karta hai aur uske peeche ki logic kya hai, to aap hamare saath deep dive kar sakte hain.

```Gleam``` programming language mein, directories ko handle karne ke liye ```:gleam: io``` library ka use kiya jata hai. Ismein, ```dir``` data type define hai jo ki directories ko represent karta hai. Ismein, directories ke liye methods bhi hote hain jaise ki ```exists``` jismein directory ke existence ko check kiya jata hai.

```Gleam
pub fn exists(dir) -> Bool
```

Yahan ```dir``` parameter mein directory ka path provide karna hoga. Agar directory maujood hai, to wo true return karega, warna false. Iske peeche ki logic, ```POSIX``` standards par based hai jo ki directories ka management karta hai.

## Dekhein Bhi

- [Gleam official documentation](https://gleam.run)
- [Gleam GitHub repository](https://github.com/gleam-lang/gleam)
- [POSIX standards for directories](https://pubs.opengroup.org/onlinepubs/007908799/xsh/sysstat.h.html)