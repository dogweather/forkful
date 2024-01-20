---
title:                "Надсилання http-запиту"
html_title:           "Arduino: Надсилання http-запиту"
simple_title:         "Надсилання http-запиту"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/uk/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Що & навіщо? 
Надсилання HTTP-запиту - це процес, подібний тому, як браузер запитує веб-сторінки від сервера. Програмісти роблять це, щоб обмінюватися даними між серверами та слідкувати за їх поведінкою.

## Як це зробити: 
В Go ви можете використовувати стандартну бібліотеку "net/http" для надсилання HTTP-запитів. Ось простий приклад:

```Go
package main

import (
	"fmt"
	"net/http"
	"io/ioutil"
)

func main() {
	resp, err := http.Get("http://example.com/")
	if err != nil {
		panic(err)
	}
	defer resp.Body.Close()
	
	body, err := ioutil.ReadAll(resp.Body)
	if err != nil {
		panic(err)
	}
	
	fmt.Println(string(body))
}
```

Після запуску програми вона зробить GET-запит до "http://example.com/" і виведе тіло відповіді у консоль.

## Поглиблено
Відправлення HTTP-запиту - це важлива частина взаємодії з веб-серверами. Це було створено ще в 1989 році коли Тім Бернерс-Лі створив World Wide Web. У Go, ви також можете використовувати альтернативні бібліотеки, такі як "gorequest" або "fasthttp", які можуть пропонувати більший функціонал або кращу продуктивність. Однак, у більшості випадків "net/http" буде достатньо.

## Дивіться також 
- "net/http" документація: https://golang.org/pkg/net/http/ 
- "gorequest" Github сторінка: https://github.com/parnurzeal/gorequest
- "fasthttp" Github сторінка: https://github.com/valyala/fasthttp