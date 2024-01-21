---
title:                "Wysyłanie żądania HTTP"
date:                  2024-01-20T17:59:50.609871-07:00
model:                 gpt-4-1106-preview
simple_title:         "Wysyłanie żądania HTTP"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/sending-an-http-request.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Wysyłamy żądanie HTTP, by porozmawiać z serwerem – zapytać o dane lub coś tam wysłać. Programiści robią to, bo to podstawa komunikacji w sieci, od stron internetowych po aplikacje.

## How to (Jak to zrobić):
```Go
package main

import (
	"fmt"
	"io/ioutil"
	"net/http"
)

func main() {
	response, err := http.Get("http://example.com")
	if err != nil {
		fmt.Printf("Błąd podczas żądania: %s\n", err)
		return
	}
	defer response.Body.Close()

	body, err := ioutil.ReadAll(response.Body)
	if err != nil {
		fmt.Printf("Nie mogę czytać danych: %s\n", err)
		return
	}

	fmt.Println(string(body))
}
```
Wynik działania:
```
<!doctype html>
<html>
<head>
    <title>Example Domain</title>
    ...
</html>
```

## Deep Dive (Głębsze zagłębienie):
HTTP pozostaje głównym protokołem do komunikacji w sieci od jego wprowadzenia w 1991 roku. Obecnie mamy HTTP/2 i nadchodzi HTTP/3, ale idea pozostaje ta sama – klient wysyła żądanie, serwer odpowiada. 

Alternatywą dla standardowej biblioteki Go do żądań HTTP jest np. `gorilla/websocket` dla WebSocketów czy pakiet `gRPC-go` dla gRPC. Ważne, aby pamiętać o bezpieczeństwie – sprawdzanie certyfikatów SSL, użycie HTTPS.

Kiedy piszemy klienta HTTP w Go, najczęściej używamy pakietu `net/http`. "Get", "Post", "PostForm" i "Do" to funkcje, które tam znajdziemy. "Do" daje najwięcej kontroli nad żądaniem HTTP – metoda, headers, body. Ważne, by pamiętać o zamykaniu `response.Body`, by uniknąć wycieków zasobów.

## See Also (Zobacz również):
- Dokumentacja Go `net/http`: https://golang.org/pkg/net/http/
- Tutorial 'Making HTTP Requests in Go': https://golangcode.com/making-http-requests/
- Artykuł o bezpieczeństwie w żądaniach HTTP w Go: https://blog.golang.org/http-tracing

Remember, to keep your coding journey enjoyable! Udanego kodzenia!