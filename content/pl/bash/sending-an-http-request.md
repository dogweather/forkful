---
title:                "Wysyłanie żądania http"
html_title:           "Bash: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "Bash"
category:             "Bash"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/bash/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Dlaczego

Wysyłanie zapytań HTTP jest ważną częścią programowania i może być wykorzystywane w wielu różnych sytuacjach. Na przykład, może być potrzebne do pobrania danych z serwera lub do komunikacji z innymi systemami.

## Jak To Zrobić

Można wysyłać żądania HTTP za pomocą polecenia `curl` w Bash. Wystarczy podać adres URL, na który chcemy wysłać żądanie. Na przykład:

```Bash
curl www.example.com
```

To spowoduje wysłanie standardowego zapytania GET do serwera www.example.com. Jeśli chcemy wysłać zapytanie innego typu, na przykład POST, możemy użyć opcji `-X`:

```Bash
curl -X POST www.example.com
```

Jeśli chcemy wysłać zapytanie z danymi, możemy użyć opcji `-d`:

```Bash
curl -d "username=example&password=secret" www.example.com/login
```

Oczywiście, to tylko podstawowe przykłady. Istnieje wiele więcej opcji, takich jak ustawianie nagłówków, używanie autoryzacji, obsługa zapytań z poświadczeniami SSL i wiele innych. Aby dowiedzieć się więcej, warto przeczytać dokumentację dla polecenia `curl` lub szukać w Internecie poradników dotyczących wysyłania zapytań HTTP z Bash.

## Deep Dive

Wysyłanie zapytań HTTP jest możliwe dzięki wykorzystaniu protokołu HTTP, który pozwala na komunikację między klientem (np. przeglądarką internetową) a serwerem. Wysyłając żądanie HTTP, klient przekazuje informacje o tym, jakie dane chce uzyskać od serwera oraz w jakim formacie ma je otrzymać. Serwer natomiast odpowiada na żądanie, przesyłając dane zgodnie z wytycznymi klienta.

Wysyłanie zapytań HTTP jest często używane w aplikacjach internetowych, w których dane muszą być pobrane lub przekazane z serwera do przeglądarki użytkownika. Dzięki temu mechanizmowi możliwe jest również korzystanie z różnych API, czyli interfejsów programistycznych, które udostępniają funkcje i dane z innych aplikacji lub serwisów.

## Zobacz również

- [Dokumentacja polecenia `curl` (ang.)](https://man7.org/linux/man-pages/man1/curl.1.html)
- [Kompletny przewodnik po wysyłaniu zapytań HTTP z Bash (ang.)](https://www.baeldung.com/curl-rest)