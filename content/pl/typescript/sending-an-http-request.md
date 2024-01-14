---
title:                "TypeScript: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Dlaczego można być zainteresowanym wysyłką żądania HTTP?

Wysyłanie żądań HTTP jest nieodłącznym elementem pracy z aplikacjami internetowymi. Pozwala ona na komunikację z serwerami i pobieranie danych. Bez tego nie da się wyobrazić przeglądania stron internetowych czy korzystania z różnych aplikacji. W tym artykule dowiecie się, dlaczego so przypuszczalnie dobrze znać ten temat.

## How To: Wysyłanie żądania HTTP w TypeScript

Aby wysłać żądanie HTTP w TypeScript należy skorzystać z metody `fetch`. Przykładowy kod może wyglądać następująco:

```TypeScript
fetch('https://api.example.com/products')
  .then(response => response.json())
  .then(data => console.log(data))
  .catch(error => console.error(error))
```

Powyższy przykład pobiera dane z serwera pod adresem `https://api.example.com/products` i wyświetla je w konsoli. Metoda `fetch` zwraca obiekt `Promise`, dlatego też korzystamy z `then` i `catch` do obsługi odpowiedzi i błędów. Pamiętajmy, że adres URL musi być poprawnie sformatowany, a jeśli jest to konieczne, możemy przesłać parametry w celu sprecyzowania naszego żądania.

## Deep Dive: Wysyłanie żądania HTTP w praktyce

Aby lepiej zrozumieć proces wysyłania żądań HTTP, warto przyjrzeć się temu w praktyce. Wysłanie żądania polega na przesłaniu odpowiedniego zestawu danych do serwera i otrzymaniu odpowiedzi w postaci tekstu lub binarnych danych. Może to być wykorzystane do pobierania plików, wysyłania formularzy czy pobierania danych z bazy.

Jednym z kluczowych elementów wysyłania żądania jest nagłówek, który zawiera informacje o żądaniu i odpowiedzi. Przykładowe nagłówki mogą zawierać typ żądania (GET, POST, PUT), kod odpowiedzi (200 - OK, 404 - Not Found) oraz dane dodatkowe, takie jak klucze API czy tokeny uwierzytelniające.

Innym ważnym elementem jest ciało (body) żądania, które zawiera właściwe dane, np. formularz z danymi użytkownika. Serwer odbiera te dane i przetwarza je, aby zwrócić odpowiedź.

Podczas wysyłania i odbierania danych, ważne jest również korzystanie z odpowiednich typów danych, np. `application/json` dla danych w formacie JSON lub `application/xml` dla danych w formacie XML.

## Zobacz również

- [Dokumentacja TypeScript - Metoda Fetch](https://www.typescriptlang.org/docs/handbook/fetch-api.html)
- [Kurs TypeScript - Wprowadzenie do HTTP](https://typescript-platzi-master.now.sh/lessons/5-wprowadzenie-do-http/)
- [Blog Programowiczny - Wysyłka żądań HTTP w TypeScript](https://programowicz.org/wysylka-zadan-http-w-typescript/)