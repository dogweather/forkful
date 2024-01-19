---
title:                "Pobieranie strony internetowej"
html_title:           "C#: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Pobieranie strony internetowej to proces, w którym twój program odzyskuje dane HTML ze strony internetowej. Programiści robią to, aby manipulować tymi danymi i uzyskać potrzebne informacje.

## Jak to zrobić:

Możemy użyć biblioteki `Gleam/httpc` do pobierania stron internetowych. Przykładowy kod i wynik wyglądają tak:

```Gleam
pub fn download(url: String) {
  case httpc.get(url) {
    Error(_) -> "Błąd przy pobieraniu strony."
    Ok(response) -> response.body
  }
}
```

Po uruchomieniu tego kodu z prawidłowym URL, otrzymamy treść strony jako wynik.

## Dogłębnie

1. **Kontekst historyczny**: Pobieranie stron internetowych jest popularne od początków internetu. Jest to podstawowy sposób, w jaki przeglądarki działają - pobierają strony i renderują je dla użytkowników.

2. **Alternatywy**: Istnieją różne metody i biblioteki do pobierania stron internetowych. W zależności od języka programowania, możemy używać `wget` lub `curl` w Shell, `urllib` lub `requests` w Pythonie itp.

3. **Szczegóły implementacji**: Biblioteka `httpc` z Gleam to prosty klient HTTP, który wykonuje podstawowe operacje HTTP takie jak GET, POST itp. Podczas pobierania strony, zapytanie GET jest wysyłane do serwera, a serwer zwraca odpowiedź, która jest zapisywana jako wynik.

## Zobacz także

1. Dokumentacja Gleam HTTPc: [https://hexdocs.pm/gleam_httpc/gleam/httpc.html](https://hexdocs.pm/gleam_httpc/gleam/httpc.html)

2. Krótki wprowadzenie do HTTP: [https://www3.ntu.edu.sg/home/ehchua/programming/webprogramming/HTTP_Basics.html](https://www3.ntu.edu.sg/home/ehchua/programming/webprogramming/HTTP_Basics.html)

3. Obsługa błędów w Gleam: [https://gleam.run/book/tour/errors.html](https://gleam.run/book/tour/errors.html)