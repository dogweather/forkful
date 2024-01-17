---
title:                "Pobieranie strony internetowej"
html_title:           "Go: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "Go"
category:             "Go"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/go/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Pobieranie strony internetowej to proces ściągania kodu źródłowego strony, który jest interpretowany i wyświetlany przez przeglądarkę internetową. Programiści mogą to robić w celu analizowania struktury strony lub wykorzystania jej treści w swoim kodzie.

## Jak:

Pobieranie strony internetowej w Go jest bardzo proste dzięki zastosowaniu pakietu "net/http". Wystarczy wykorzystać funkcję "Get" i przekazać jej adres URL strony, którą chcemy pobrać.

```Go
resp, err := http.Get("https://www.example.com")
if err != nil {
  // obsłuż błąd
}
defer resp.Body.Close()

body, err := ioutil.ReadAll(resp.Body)
if err != nil {
  // obsłuż błąd
}

fmt.Println(string(body))
```

Przykładowe wyjście: ```<!doctype html> <html> <head> <title>Example Domain</title> [...]

## Głębszy zanurzenie:

Pobieranie stron internetowych jest procesem, który jest wykorzystywany od lat przez programistów. Dawniej używano do tego celu protokołu FTP lub specjalnych programów do pobierania stron, jednak teraz wiele języków programowania (w tym Go) oferuje wbudowane funkcje do tego celu.

Alternatywnym sposobem na pobieranie stron internetowych w Go jest użycie pakietu "net/url" oraz funkcji "Parse" do przetwarzania adresu URL strony na obiekt URL.

Implementacyjnie, pobieranie strony internetowej w Go wykorzystuje protokół HTTP oraz standardowe funkcje GET i POST. Więcej informacji na temat implementacji można znaleźć w dokumentacji języka Go.

## Zobacz też:

- Dokumentacja pakietu "net/http": https://golang.org/pkg/net/http/
- Dokumentacja pakietu "net/url": https://golang.org/pkg/net/url/
- Przykładowe kody pobierania stron internetowych w Go: https://github.com/topics/go-web-scraping