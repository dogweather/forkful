---
title:                "Go: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Dlaczego

W dzisiejszych czasach, większość aplikacji korzysta z rozproszonej architektury, co oznacza że różne komponenty systemu muszą ze sobą komunikować. W tym celu wykorzystywane są często protokoły sieciowe, takie jak HTTP. W tym artykule dowiesz się dlaczego wysyłanie żądań HTTP jest tak ważne w programowaniu w języku Go.

## Jak to zrobić

Aby wysłać żądanie HTTP w języku Go, musimy najpierw zaimportować pakiet "net/http". Następnie możemy wykorzystać funkcję "Get" aby wykonać zapytanie do wybranej ścieżki URL. Poniżej znajduje się przykładowy kod:

```Go
package main

import (
	"fmt"
	"net/http"
)

func main() {
	response, err := http.Get("https://www.example.com")
	if err != nil {
		fmt.Println(err)
	}
	defer response.Body.Close()

	fmt.Println(response.Status)
}
```

Powyższy kod wyśle żądanie GET do strony "https://www.example.com" i zwróci odpowiedź, w której znajduje się między innymi status żądania. Innym przydatnym narzędziem jest pakiet "net/http/httputil", który pozwala na bardziej szczegółowe analizowanie żądań i odpowiedzi. Możliwości jest wiele, a najlepszym sposobem na naukę jest eksperymentowanie z różnymi funkcjami dostępnymi w pakiecie "net/http".

## Głębsza analiza

Aby lepiej zrozumieć działanie funkcji "Get", warto przejrzeć jej implementację w pakiecie "net/http". Możemy zauważyć, że częścią żądania jest również obiekt typu "Request", który zawiera parametry takie jak metoda, adres URL, nagłówki czy ciało żądania. Ponadto, klikając odwołanie do dokumentacji, możemy znaleźć więcej informacji o innych funkcjach dostępnych w pakiecie "net/http".

## Zobacz też

- [Dokumentacja pakietu "net/http"](https://golang.org/pkg/net/http/)
- [Oficjalny kurs Go od Google na platformie Coursera](https://www.coursera.org/learn/google-go)
- [Artykuł o tworzeniu sieciowych aplikacji w języku Go](https://hackernoon.com/network-programming-with-go-947f8ec5c417)