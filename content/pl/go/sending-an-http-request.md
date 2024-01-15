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

## Dlaczego

Wysyłanie żądania HTTP jest jedną z podstawowych umiejętności, które powinien posiadać każdy programista w języku Go. Dzięki temu możemy komunikować się z innymi serwerami i aplikacjami, pobierać i wysyłać dane, co jest niezbędne w wielu zastosowaniach internetowych.

## Jak to zrobić

Aby wysłać żądanie HTTP w języku Go, używamy funkcji `http.Get()` z pakietu `net/http`. Przykładowy kod wygląda następująco:

```Go
res, err := http.Get("https://www.example.com")
if err != nil {
  // obsługa błędu
}
defer res.Body.Close() // zamykamy ciało odpowiedzi po zakończeniu funkcji

// wyciągamy potrzebne informacje z odpowiedzi
body, err := ioutil.ReadAll(res.Body)
if err != nil {
  // obsługa błędu
}
fmt.Println(string(body))
```

W powyższym przykładzie odwołujemy się do adresu URL `https://www.example.com` i pobieramy z niego ciało odpowiedzi. Następnie wypisujemy je na konsolę. Ważne jest, aby zawsze pamiętać o zamknięciu ciała odpowiedzi za pomocą funkcji `res.Body.Close()`.

## Głębsze zanurzenie

Istnieje wiele innych opcji i ustawień, które możemy wykorzystać przy wysyłaniu żądań HTTP w języku Go. Przykładowo, możemy zmienić nagłówki żądania, określić limit czasu oczekiwania na odpowiedź czy obsługiwać przekierowania.

Aby dowiedzieć się więcej na temat wysyłania żądań HTTP w języku Go, polecamy zapoznać się z dokumentacją pakietu `net/http` oraz przetestować różne opcje i funkcjonalności samodzielnie.

## Zobacz także

- Dokumentacja pakietu `net/http`: https://golang.org/pkg/net/http/
- Przykładowe programy w języku Go: https://gobyexample.com/
- Kurs języka Go w Codecademy: https://www.codecademy.com/learn/learn-go