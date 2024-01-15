---
title:                "स्टैंडर्ड एरर में लिखना"
html_title:           "Fish Shell: स्टैंडर्ड एरर में लिखना"
simple_title:         "स्टैंडर्ड एरर में लिखना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/writing-to-standard-error.md"
---

{{< edit_this_page >}}

## Kyun

Dosto, kya aap Fish Shell ka istemal karte hai? Agar haan, toh aapko pata hoga ki yeh ek bahut hi powerful aur customizable shell hai. Par kya aapko pata hai ki aap Fish Shell mein standard error mein bhi likh sakte hai? Haan, aapne sahi suna! Is article mein hum aapko batayenge ki kaise aap standard error mein likh kar apne code ko aur bhi efficient bana sakte hai. Toh chaliye shuru karte hai!

## Kaise Karein

Jaise ki hum sab jante hai ki Shell commands ko execute karne ke baad hume ek output milta hai, jo ki humare Terminal par likha hua hota hai. Lekin kabhi kabhi hume kuch errors bhi milte hai, jaise ki "Command not found" ya fir "Invalid argument". In errors ko hum standard error kehte hai. Agar hum chahe toh hum in errors ko apne code se alag kar sakte hai. Iske liye hum standard error mein likh sakte hai.

```Fish Shell
echo "Hello World!" >&2

Output: Hello World!
```

Is example mein humne `echo` command ko use kiya hai jiske output ko standard error mein likhne ka instruction hai `>&2`. Is tarah se hum apne code mein multiple commands ke beech errors ko identify kar sakte hai.

## Gehra Sach

Standard error mein likhne ki madad se hum apne code ko aur bhi organized aur readable bana sakte hai. Isse hume errors ko identify karne aur fix karne mein aasani hoti hai. Aur saath hi saath, hum apne code ko debug karne mein bhi aasani pa sakte hai.

Iske alawa, standard error mein likhne se hume errors ki sankhya aur details ko track karne ka bhi option milta hai. Isse humare code ko optimize karne mein madad milti hai.

Iske alawa, upyog karne ka ek aur fayda hai ki hum apne code mein koi bhi changes or updates kar sakte hai aur firse apne errors ka track rakh sakte hai. Isse hum apne code ko robust bana sakte hai.

Toh dosto, is article mein humne dekha ki kaise hum standard error mein likh kar apne code ko aur bhi efficient bana sakte hai. Agar aap Fish Shell ka istemal nahi karte hai, toh yeh ek accha mauka hai usse try karne ka! Hum umeeed karte hai ki yeh article aapke liye helpful hoga. Dhanyavaad!

## Dekhein Bhi

- [Official Fish Shell website](https://fishshell.com/)
- [Fish Shell documentation](https://fishshell.com/docs/current/index.html)
- [List of Fish Shell functions and commands](https://fishshell.com/docs/current/commands.html)