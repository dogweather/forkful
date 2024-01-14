---
title:                "Javascript: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Dlaczego

Przesyłanie żądań HTTP stanowi nieodłączną część programowania w języku JavaScript. Pozwala nam na komunikację z serwerami, pobieranie danych i wykonywanie działań na zewnętrznych zasobach. Jest to kluczowy element w tworzeniu interaktywnych aplikacji internetowych.

## Jak to zrobić

Aby wysłać żądanie HTTP w JavaScript, musimy najpierw utworzyć obiekt XMLHTTPRequest za pomocą konstruktora `new XMLHttpRequest()`. Następnie, musimy określić metodę (np. GET, POST, PUT) oraz adres URL docelowego zasobu.

```Javascript
const request = new XMLHttpRequest();
request.open('GET', 'https://example.com'); // utworzenie żądania GET do zasobu 'example.com'
request.send();
```

Jeśli chcemy przesłać dane, możemy użyć metody `.send()` wraz z parametrem zawierającym dane w formacie JSON lub przesyłając formularz.

W przypadku pomyślnego wykonania żądania, możemy uzyskać dostęp do zwrotki (odpowiedzi serwera) za pomocą właściwości `responseText` lub `responseXML` w zależności od formatu otrzymanych danych.

```Javascript
request.onload = function() {
  console.log(request.responseText); // wyświetlenie zwrotki w konsoli
}
```

## Zanurzenie

Istnieje wiele zaawansowanych możliwości związanych z wysyłaniem żądań HTTP w JavaScript, takich jak korzystanie z biblioteki `fetch`, obsługa błędów, ustawianie nagłówków oraz przesyłanie danych w różnych formatach (np. FormData). Dodatkowo, istnieją także narzędzia, które ułatwiają tworzenie i testowanie żądań HTTP, takie jak `Postman` czy `Insomnia`.

## Zobacz także

- [Dokumentacja Mozilla Developer Network na temat wysyłania żądań HTTP w JavaScript](https://developer.mozilla.org/pl/docs/Web/API/XMLHttpRequest/Sending_and_Receiving_Binary_Data)
- [Dokumentacja biblioteki Fetch](https://developer.mozilla.org/pl/docs/Web/API/Fetch_API)
- [Poradnik na temat przesyłania danych za pomocą FormData](https://developer.mozilla.org/en-US/docs/Web/API/FormData/Using_FormData_Objects)
- [Postman](https://www.postman.com/)
- [Insomnia](https://insomnia.rest/)