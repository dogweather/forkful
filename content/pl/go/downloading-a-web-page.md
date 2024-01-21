---
title:                "Pobieranie strony internetowej"
date:                  2024-01-20T17:44:03.243086-07:00
model:                 gpt-4-1106-preview
simple_title:         "Pobieranie strony internetowej"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Pobieranie strony internetowej to proces ściągania danych z internetu na swój komputer. Programiści robią to, by przetworzyć informacje, testować aplikacje czy analizować treści.

## How to: (Jak to zrobić:)
```Go
package main

import (
	"fmt"
	"io"
	"net/http"
	"os"
)

func main() {
	// Adres URL strony do pobrania
	url := "http://example.com"

	// Wykonanie żądania HTTP GET
	response, err := http.Get(url)
	if err != nil {
		fmt.Println("Błąd podczas pobierania strony:", err)
		return
	}
	defer response.Body.Close()

	// Otworzenie pliku do zapisu
	file, err := os.Create("example.html")
	if err != nil {
		fmt.Println("Błąd podczas tworzenia pliku:", err)
		return
	}
	defer file.Close()

	// Kopiowanie treści do pliku
	_, err = io.Copy(file, response.Body)
	if err != nil {
		fmt.Println("Błąd podczas kopiowania treści:", err)
		return
	}

	fmt.Println("Strona została pobrana pomyślnie.")
}
```
Po uruchomieniu, program zapisze zawartość strony z example.com do pliku "example.html".

## Deep Dive (Dogłębna analiza)
Pobieranie stron internetowych sięga początków Internetu, kiedy to korzystano z protokołów takich jak FTP. Dziś używa się głównie HTTP/HTTPS. Alternatywą dla standardowej biblioteki `net/http` w Go jest użycie zewnętrznych pakietów jak `gorilla/websocket` dla operacji czasu rzeczywistego czy `colly` dla scrapingu. Podczas implementacji ważne jest, aby zwrócić uwagę na obsługę przekierowań, limit czasu połączenia oraz poprawne zarządzanie sesją, zwłaszcza przy stronach dynamicznych.

## See Also (Zobacz również)
- Dokumentacja Go: https://golang.org/pkg/net/http/
- Wprowadzenie do pakietu `colly`: http://go-colly.org/
- Przewodnik po `gorilla/websocket`: https://github.com/gorilla/websocket