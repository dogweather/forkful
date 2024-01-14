---
title:                "Fish Shell: वर्तमान तिथि प्राप्त करना"
simple_title:         "वर्तमान तिथि प्राप्त करना"
programming_language: "Fish Shell"
category:             "Fish Shell"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/fish-shell/getting-the-current-date.md"
---

{{< edit_this_page >}}

## Kyu 
Kya aapko pata hai ki apne computer ko pata hai ki aaj konsa din hai? Agar nahi toh aapke liye kya mauka hai apne Fish Shell programming skills ko sudharne ka by pata karne ke liye current date. Is blog post mein hum aapko current date pata karne ke bare mein batayenge.

## Kaise Karein
Fish Shell mein, ham `date` command ka istemal karke current date pata kar sakte hain. Is command ko use karne ke liye aapko bas command prompt pe `date` likhna hai aur Enter dabana hai. Yeh command aapko current date aur time dikhayegi. Agar aap sirf date ya sirf time pata karna chahte hain, toh aap `date +%D` ya `date +%T` command ka istemal kar sakte hain.

```Fish Shell
date
```
Output:
```Fish Shell
Thu Aug 20 20:08:59 IST 2020
```

Agar aap apne hisaab se date aur time format karna chahte hain, toh aap `date` command ke sath [strftime](https://fishshell.com/docs/current/cmds/date.html#strftime) ka istemal kar sakte hain. Isse aap apni pasand ka date aur time format bana sakte hain.

```Fish Shell
date "+%A, %B %d, %Y"
```
Output:
```Fish Shell
Thursday, August 20, 2020
```

## Deep Dive
Jab ham `date` command ko istemal karte hain, toh yeh system ki date aur time settings ko use karta hai. Yeh command apne default format mein date aur time dikhata hai jo aapke computer ki language aur region settings pe depend karta hai. Isse aapki local time zone ke hisab se current date aur time milta hai.

Agar aapko international date aur time format jaise ISO-8601 format mein date show karna chahte hain, toh aap `date -u +%Y-%m-%dT%H:%M:%SZ` command ka istemal kar sakte hain.

## Dekhenge
Yadi aapko Fish Shell or programming ke bare mein adhik jaankari chahiye, toh neeche diye gaye link dekhe:

- [Official Fish Shell Documentation](https://fishshell.com/docs/current/)
- [Fish Shell Tutorials on YouTube](https://www.youtube.com/playlist?list=PLIxLil0ChYJ6bozMKd8RzAH1Up23LUa8-)
- [Fish Shell Tutorials on FreeCodeCamp](https://www.freecodecamp.org/news/search/?query=fish%20shell)