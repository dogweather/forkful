---
title:                "Wysyłanie żądania HTTP"
html_title:           "C++: Wysyłanie żądania HTTP"
simple_title:         "Wysyłanie żądania HTTP"
programming_language: "C++"
category:             "C++"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/cpp/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli chcesz zintegrować swoje oprogramowanie z innymi aplikacjami lub serwisami internetowymi, wysyłanie żądań HTTP może być niezbędnym krokiem. Dzięki temu narzędziu możesz w prosty sposób komunikować się z innymi systemami, pobierać i przesyłać dane oraz wykonywać różne operacje.

## Jak to zrobić

Aby wysłać żądanie HTTP w języku C++, musisz najpierw zaimportować bibliotekę "HTTPRequest.h". Następnie musisz utworzyć obiekt żądania i określić jego metodę (GET, POST, itp.), adres URL oraz nagłówki. W przykładzie poniżej wyślemy GET request do strony "example.com", pobierzemy zawartość odpowiedzi i wyświetlimy ją na ekranie:

```C++
#include "HTTPRequest.h"

int main()
{
    // Utwórz obiekt żądania HTTP
    http::URLRequest request("http://example.com", http::Method::GET);

    // Dodaj nagłówki (opcjonalnie)
    request.addHeader("Content-Type", "application/json");

    // Wyślij żądanie i pobierz odpowiedź
    http::Response response = request.send();

    // Wyświetl zawartość odpowiedzi
    std::cout << response.getBody() << std::endl;

    return 0;
}
```

Przykładowy output:

```
<!DOCTYPE html>
<html>
<head>
    <title>Welcome to Example.com</title>
</head>
<body>
    <h1>Hello world!</h1>
</body>
</html>
```

Tak jak w tym przykładzie, możesz wykorzystać różne metody HTTP, takie jak GET, POST, PUT, DELETE, aby przesyłać różne rodzaje danych i wykonywać różne operacje.

## Deep Dive

Istnieje wiele bibliotek i narzędzi do wysyłania żądań HTTP w języku C++, takich jak cURL, CPPRESTSDK czy Poco. Każde z nich ma swoje własne metody i interfejsy, więc warto przetestować kilka z nich i wybrać ten, który najlepiej odpowiada Twoim potrzebom.

Podczas wysyłania żądania warto również zwrócić uwagę na obsługę błędów i wyjątków. Zawsze należy zapewnić, że połączenie zostało nawiązane i odpowiedź została poprawnie odebrana przed przetwarzaniem danych.

## Zobacz także

- [Dokumentacja biblioteki "HTTPRequest"](https://github.com/elnormous/HTTPRequest)
- [Porównanie popularnych bibliotek HTTP w C++](https://blog.knatten.org/2019/09/26/a-comparison-of-cpp-http-libraries/)
- [Krótki tutorial o wysyłaniu żądań HTTP w C++](https://www.shahmoradi.org/ECE1552/newLabs/computerNetworks/howToSendHTTPrequest/tutorialOfHttp.htm)