---
title:                "Pobieranie strony internetowej"
html_title:           "C#: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Pobieranie strony internetowej polega na zapisaniu jej zawartości lokalnie na dysku twardym. Programiści robią to, aby analizować strukturę strony, pobierać dane (web scraping) lub tworzyć kopie zapasowe stron.

## Jak to zrobić:

```Go
package main

import (
	"io"
	"net/http"
	"os"
)

func main() {
	resp, err := http.Get("http://example.com")
	if err != nil {
		panic(err)
	}
	defer resp.Body.Close()

	file, err := os.Create("example.html")
	if err != nil {
		panic(err)
	}
	defer file.Close()

	_, err = io.Copy(file, resp.Body)
	if err != nil {
		panic(err)
	}
}
```

Uruchamiając powyższy kod, pobierzemy zawartość strony `http://example.com` i zapiszemy ją do pliku `example.html` na naszym dysku twardym.

## Na głębsze wody

Początki pobierania stron internetowych sięgają czasów, gdy internet był jeszcze w powijakach - to właśnie wtedy zaczęto tworzyć pierwsze web scrapery. Alternatywą dla powyższego rozwiązania jest użycie pakietu `goquery`, który umożliwia nie tylko pobranie strony, ale także wygodne przeszukiwanie jej zawartości. Szczegółowość implementacji pobierania stron internetowych może się znacznie różnić - od prostych skryptów do złożonych systemów z możliwością omijania zabezpieczeń i obsługą różnych formatów stron.

## Zobacz też

[`http.Get`](https://golang.org/pkg/net/http/#Client.Get) - Dokumentacja funkcji `http.Get` z pakietu `http`.

[`io.Copy`](https://golang.org/pkg/io/#Copy) - Dokumentacja funkcji `io.Copy` z pakietu `io`.

[`os.Create`](https://golang.org/pkg/os/#Create) - Dokumentacja funkcji `os.Create` z pakietu `os`.

[`goquery`](https://github.com/PuerkitoBio/goquery) - Pakiet `goquery` umożliwiający wygodne przeszukiwanie zawartości stron.