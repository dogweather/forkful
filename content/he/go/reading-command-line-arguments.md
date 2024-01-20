---
title:                "קריאה של ארגומנטים משורת הפקודה"
html_title:           "C#: קריאה של ארגומנטים משורת הפקודה"
simple_title:         "קריאה של ארגומנטים משורת הפקודה"
programming_language: "Go"
category:             "Go"
tag:                  "Files and I/O"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/he/go/reading-command-line-arguments.md"
---

{{< edit_this_page >}}

## מה ולמה?
קריאת ארגומנטים ממסוף היא הקניית ערכים ממסוף משתמש באמצעות שפת תכנות שכמו Go. זה מאפשר לנו להפוך את הקוד שלנו ליישומים פרטיים יותר ומותאמים אישית.

## איך:
ב-Go, אנחנו יכולים להשתמש בספריית `os` למען מטרה זו.

```Go
package main

import (
    "fmt"
    "os"
)

func main() {
    argsWithProg := os.Args
    argsWithoutProg := os.Args[1:]
   
    arg := os.Args[3]
   
    fmt.Println(argsWithProg)
    fmt.Println(argsWithoutProg)
    fmt.Println(arg)
}
```
פלט עשוי להיראות כך:

```Go
[./argstest arg1 arg2 arg3]
[arg1 arg2 arg3]
arg3
```

## לעומק:
עד ש-Go הגיע, היה שם מעט שליטה או שימוש מנותב בערכים המעברים מהמסוף. Go אפשר לתכנתים לפעול בצורה יותר שלמה בקרבת המשתמש. אפשרויות אלטרנטיביות שמתקיימות ב- Go מאפשרות למרשר פלט (דרך flag או pflag למשל). 

## ראה גם:
* [os.Args ב-Tour of Go](https://tour.golang.org/list)
* [os package in Go by Example](https://gobyexample.com/command-line-arguments)
* [בלוג Go של Dave Cheney בבניית פרמטרים מילוליים](https://dave.cheney.net/2019/09/24/be-wary-of-functions-which-take-several-parameters-of-the-same-type)