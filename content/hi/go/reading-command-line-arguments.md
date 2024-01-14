---
title:    "Go: कम्प्यूटर प्रोग्रामिंग में कमांड लाइन आर्ग्यूमेंट्स पढ़ना"
keywords: ["Go"]
editURL:  "https://github.com/dogweather/forkful/blob/master/content/hi/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

# Kyon
Command line arguments mein ruchi prakat karne ki wajah hai ki yeh Go programming language mein saral aur prabhavshali tarike se user input ko handle karti hai. Iska upyog command line utilities, scripting aur application development mein kiya ja sakta hai.

# Kaise
Command line arguments ko Go mein padhna bahut aasan hai. Pehle, hum `os` package ko import karenge.
```Go
import "os"
```
Fir, `os.Args` variable mein humare command line arguments store hote hain. Iska upyog hum `range` keyword ke saath use kar sakte hain.
```Go
for _, arg := range os.Args {
    fmt.Println(arg)
}
```
Yahan, humne `_` underscore ko ignore kar diya hai kyunki humein `os.Args` ke pehle argument ko chhodkar baaki sabhi ko print karna hai. Is code ka sample output niche diya gaya hai:
```
go run main.go hello world
hello
world
```

# Gehri Jhalak
Command line arguments padhne ke alawa, hum unhe manipulate bhi kar sakte hain. `flag` package ki madad se hum command line options aur flags ko bhi handle kar sakte hain.
```Go
import "flag"

limit := flag.Int("limit", 10, "Set the limit for output")
flag.Parse()

for i, arg := range os.Args {
    if i == *limit {
        break
    }
    fmt.Println(arg)
}
```
Yahan, hum `flag` package se `Int` method ka upyog karke `limit` flag ko set kar sakte hain. Ab, `flag.Parse()` se hum command line arguments ko parse kar sakte hain aur `*limit` se uski value prapt kar sakte hain. Is code ka sample output niche diya gaya hai:
```
go run main.go hello world -limit=1
hello
```

# Dekhein Bhi
- [Official Go documentation on command line arguments](https://golang.org/pkg/os/#pkg-variables)
- [A tutorial on reading and parsing command line arguments in Go](https://www.digitalocean.com/community/tutorials/how-to-read-command-line-arguments-in-golang)
- [An in-depth article on working with flags in Go](https://blog.gopheracademy.com/advent-2014/advanced-flags/)
- [Examples of using command line arguments in real-world applications](https://github.com/urfave/cli/wiki/Articles-%26-Talks)