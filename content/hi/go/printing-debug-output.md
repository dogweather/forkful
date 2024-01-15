---
title:                "डीबग आउटपुट प्रिंट करना"
html_title:           "Go: डीबग आउटपुट प्रिंट करना"
simple_title:         "डीबग आउटपुट प्रिंट करना"
programming_language: "Go"
category:             "Go"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/printing-debug-output.md"
---

{{< edit_this_page >}}

## Why

Kabhi kabhi hamare code me kuch gadbad ho jati hai aur hame pata nahi chalta ki problem kaha hai. Debug output print karne se hum apne code ki execution flow ko track kar sakte hai aur sahi jagah par sahi value aa rahi hai ya nahi, isse hamare code ko debug karna aasan ho jata hai. Debug output printing ke through hum apne code ka performance bhi improve kar sakte hai.

## How To

Debug output printing Go programming language me bahut hi easy hai. Ham `fmt` package ka use karke simple `Println()` function ka use kar sakte hai. Iske liye hame `import` statement me `fmt` package ko add karna hoga.

Iske baad hame apne code ke kisi bhi jagah par `Println()` function ka use karke desired value print kar sakte hai. Isse hame pata chalta hai ki code kis point par execute ho raha hai aur uss point par sahi value aa rahi hai ya nahi.

```Go
import "fmt"

fmt.Println("Hello World!")
```

Is code snippet me `Println()` function `fmt` package ka ek part hai jisse hum apne console me output print kar sakte hai. Agar hume multiple variables ka value check karna hai to hum `Printf()` function ka use kar sakte hai.

```Go
import "fmt"

age := 21
name := "John"

fmt.Printf("My name is %s and I am %d years old.", name, age)
```

Is tarah se hum `Println()` aur `Printf()` function ka use karke apne code me debug output print kar sakte hai. Isse hame code ke sahi execution flow ka pata chalta hai.

## Deep Dive

Debug output printing ke liye Go language me `fmt` package ke alawa aur bhi bahut sare tools available hai jaise `log` package, `syscall` package, etc. In tools ka use karke hum apne code ke performance ko bhi monitor kar sakte hai.

Debug output printing ka main goal hai hume pata chal jaye ki code kis point par execute ho raha hai, uss point par sahi value aa rahi hai ya nahi. Isse hum future me code ko improve kar sakte hai aur bugs ko fix kar sakte hai.

## See Also

- [Official Go documentation for fmt package](https://golang.org/pkg/fmt/)
- [Debugging in Go with log package](https://golangbot.com/debugging-go/)
- [Using syscall package for debug output printing](https://tutorialedge.net/golang/go-debugging-syscall-package/)