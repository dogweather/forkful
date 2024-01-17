---
title:                "Pobieranie strony internetowej"
html_title:           "Gleam: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "Gleam"
category:             "Gleam"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/gleam/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Co i Dlaczego?

Pobieranie strony internetowej to proces pozyskiwania informacji z sieci. Programiści wykorzystują to narzędzie, aby uzyskać dostęp do danych z różnych stron internetowych, co pozwala im na tworzenie aplikacji, które wymagają dostępu do aktualnych informacji z sieci.

## Jak to zrobić:

W duchu Gleam uruchomić (zagnieździć) kod pod spodem i wypisać wiadomości otrzymane ze strony internetowej do konsoli:

```Gleam

let url = "https://www.example.com"

Url.fetch(url)
    .then(
        Fn.compose_ok(|_string| {
            IO.print("Pobieranie strony się powiodło!")
        })
    )
    .catch(
        Fn.compose_err(|_error| {
            IO.print("Pobieranie strony nie powiodło się.")
        })
    )
```

## Głębsza Analiza:

Pobieranie stron internetowych jest ważnym narzędziem dla programistów od początków Internetu. Istnieje wiele alternatywnych sposobów pobierania danych z sieci, takich jak wykorzystanie protokołu HTTP, bibliotek zewnętrznych lub specjalnych programów. W Gleam, używamy funkcji "Url.fetch" do pobierania stron internetowych poprzez protokół HTTP, który jest powszechnie stosowany w przesyłaniu informacji w sieci.

## Zobacz też:

- Dokumentacja Gleam: https://gleam.run/documentation/
- Przykłady pobierania stron internetowych w Gleam: https://github.com/gleam-lang/example-projects/tree/master/url-fetch