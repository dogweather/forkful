---
title:                "Wysyłanie żądania http"
html_title:           "Go: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

### Co i dlaczego?

Wysyłanie żądania HTTP jest często używaną metodą programistyczną, polegającą na przesyłaniu żądania do serwera w celu pobrania lub przetworzenia danych. Programiści często korzystają z tej metody w swoich projektach, ponieważ pozwala ona na interakcję z serwerem i przesyłanie danych.

### Jak to zrobić:

```Go
package main

import (
	"fmt"
	"io/ioutil"
	"net/http"
)

func main() {
	url := "https://example.com"
	req, _ := http.NewRequest("GET", url, nil)

	res, _ := http.DefaultClient.Do(req)

	defer res.Body.Close()
	body, _ := ioutil.ReadAll(res.Body)

	fmt.Println(string(body))
}
```

W powyższym przykładzie wykorzystujemy paczkę `net/http` oraz funkcję `NewRequest` do utworzenia żądania HTTP typu GET do strony internetowej "example.com". Następnie używamy klienta `http.DefaultClient`, aby wysłać żądanie i otrzymać odpowiedź. Za pomocą `ioutil` przetwarzamy odpowiedź i wyświetlamy ją na ekranie.

### Wnikliwe spojrzenie:

Wysyłanie żądania HTTP jest często stosowane w programowaniu, zwłaszcza do komunikacji między klientem a serwerem. Alternatywą dla biblioteki standardowej `net/http` jest popularna paczka `curl`, która oferuje więcej funkcjonalności i opcji konfiguracji, ale wymaga instalacji zewnętrznego oprogramowania. Wszystkie szczegóły dotyczące wysyłania żądań HTTP są dokładnie opisane w dokumentacji paczki `net/http`, więc warto zapoznać się z nią w celu lepszego zrozumienia tego procesu.

### Zobacz również:

- [Dokumentacja paczki net/http w języku polskim](http://golang.org/pkg/net/http/)
- [Oficjalny poradnik Go](https://tour.golang.org/welcome/1) dla początkujących programistów
- [Porównanie bibliotek net/http i curl w języku angielskim](https://medium.com/@nate510/don-t-use-go-s-default-http-client-4804cb19f779)