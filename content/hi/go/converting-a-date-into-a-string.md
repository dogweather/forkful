---
title:                "तारीख को स्ट्रिंग में रूपांतरण करना"
html_title:           "Go: तारीख को स्ट्रिंग में रूपांतरण करना"
simple_title:         "तारीख को स्ट्रिंग में रूपांतरण करना"
programming_language: "Go"
category:             "Go"
tag:                  "Dates and Times"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/hi/go/converting-a-date-into-a-string.md"
---

{{< edit_this_page >}}

## Kyun
Kya aapne kabhi socha hai ki aapke computer ya mobile mein dikhayi jane wali tarikh aur samay, humare najdiki environment mein kahan se aati hai? Isse judi ek aur zaroori sawal hai, ki kaise hum apne code mein dates ko numbers ke jagah alag alag format mein represent kar sakte hain? Yeh sab possible hai *Converting a date into a string* ka use karke. Is article mein hum aapko batayenge ki Go language mein hum date ko string mein kaise convert kar sakte hain aur iski kyuki hai.

## Kaise Kare
```Go
package main

import (
	"fmt"
	"time"
)

func main() {
	// Current date and time
	t := time.Now()
	// Date to string conversion
	dateStr := t.Format("01-02-2006")
	// Output
	fmt.Printf("Converted date string: %s", dateStr)
}
```
Is code mein hum `time` library ka use karke current date and time ko lete hain. Uske baad hum `Format()` function ka use karke date ko string mein convert kar lete hain. Yahan `"01-02-2006"` format "01" ko month ke liye, "02" ko date ke liye aur "2006" ko year ke liye define karta hai. Aap is format ko apne according modify kar sakte hain. Iske baad hum uss string ko print karte hain.

Output:
```Go
Converted date string: 08-27-2021
```

## Deep Dive
Jaise ki humne dekha, Go mein hum `time` library ka use karke date ko string mein convert kar sakte hain. Yeh conversion humari programming mein date and time ko display karne ke liye bahut useful hoti hai. Isse hum apne code ko user-friendly bana sakte hain. Ismein hum ek format specifier string ko expect karte hain jisse hum date aur time ko display kar sake. Ismein hum kuch predefined specifiers use kar sakte hain jaise "Mon", "Jan", "2006" etc. Lekin agar hame custom format chahiye toh hum `time` ke methods ka bhi use kar sakte hain jaise `t.Month()`, `t.Date()`, `t.Year()` etc.

## Dekhiye Bhi
- [Date and Time in Go](https://gobyexample.com/time)
- [Working with Date and Time in Go](https://www.callicoder.com/golang-working-with-datetime/)
- [Go Standard Library: Time](https://golang.org/pkg/time/)

## Aakhir Mein
Ummid hai ki aapko is article se Go language mein date ko string mein convert karne ka process samajh aa gaya hoga. Yadi aapko koi bhi sawal ho toh hume comment section mein pooch sakte hain. Happy Coding!