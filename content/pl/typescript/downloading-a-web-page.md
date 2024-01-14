---
title:                "TypeScript: Ściąganie strony internetowej"
simple_title:         "Ściąganie strony internetowej"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Dlaczego

Pobieranie stron internetowych może być bardzo przydatne w wielu sytuacjach, takich jak tworzenie aplikacji do analizowania danych lub budowania internetowych katalogów. Dzięki nauce programowania w TypeScript możesz łatwo pobrać zawartość strony internetowej i wykorzystać ją w swoich projektach. W tym artykule dowiesz się, jak to zrobić.

## Jak to zrobić

Pierwszym krokiem jest zaimportowanie modułu *http* w TypeScript:

```TypeScript
import * as http from 'http';
```

Następnie musisz utworzyć zapytanie do żądanej strony internetowej i przypisać go do zmiennej:

```TypeScript
const request = http.request('http://example.com', (response) => { 
  // kod odpowiedzi
});
```

W tym przykładzie, korzystamy z funkcji *http.request* i przekazujemy jej adres URL strony, którą chcemy pobrać. Drugi parametr to funkcja zwrotna, która zostanie wywołana z odpowiedzią serwera.

Teraz, musimy przetworzyć otrzymaną odpowiedź ze strony internetowej. W funkcji zwrotnej możemy wykorzystać metodę *on* do nasłuchiwania zdarzeń, takich jak *data* (otrzymanie danych) i *end* (zakończenie przetwarzania):

```TypeScript
response.on('data', (data) => {
  // przetwarzanie danych
});

response.on('end', () => {
  // zakończenie przetwarzania
})
```

Możemy wyświetlić pobrane dane w konsoli, używając metody *toString()*:

```TypeScript
response.on('data', (data) => {
  console.log(data.toString());
});
```

Teraz, gdy mamy już dane, możemy zrobić z nimi co tylko chcemy - np. zapisać je do pliku lub wykorzystać w swojej aplikacji.

## Deep Dive

Teraz już wiesz, jak pobrać zawartość strony internetowej w TypeScript, ale może chcesz się dowiedzieć więcej o tym procesie. Pobieranie stron internetowych wykorzystuje protokół HTTP, który definiuje, w jaki sposób serwer wysyła żądania i otrzymuje odpowiedzi. W kodzie naszego przykładu, wykorzystujemy funkcję *http.request* do wysłania żądania GET na adres URL. Następnie, w funkcji zwrotnej, możemy odczytać odpowiedź serwera dzięki temu, że ona też jest strumieniem danych. W tym przykładzie, wykorzystujemy metodę *data* do nasłuchiwania i przetwarzania otrzymanych danych. Gdy kończymy otrzymywać dane, funkcja zwrotna *end* jest wywoływana.

## Zobacz też

* [Dokumentacja TypeScript](https://www.typescriptlang.org/docs/)
* [Moduł 'http' w Node.js](https://nodejs.org/api/http.html)
* [Tutorial - Jak pobrać stronę internetową w Node.js](https://www.twilio.com/blog/2017/08/http-requests-in-node-js.html)