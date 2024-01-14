---
title:                "Javascript: Wysyłanie żądania http z podstawową autoryzacją"
simple_title:         "Wysyłanie żądania http z podstawową autoryzacją"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/sending-an-http-request-with-basic-authentication.md"
---

{{< edit_this_page >}}

## Dlaczego

Jeśli pracujesz jako programista, prawdopodobnie spotkałeś się już z pojęciem autoryzacji. Jest to proces uwierzytelniania użytkownika lub aplikacji do dostępu do określonych danych lub funkcji. Jedną z metod autoryzacji jest podstawowa autoryzacja HTTP, która wykorzystuje nagłówek HTTP o nazwie "Autorization" do przesyłania informacji uwierzytelniających. W tym artykule zobaczymy, dlaczego warto wykorzystywać podstawową autoryzację HTTP w żądaniach.

## Jak to zrobić

Podstawowa autoryzacja HTTP jest stosowana w przypadkach, gdy potrzebujemy szybkiej i prostszej metody autoryzacji w naszych żądaniach. Może to być na przykład wykorzystane w przypadku dostępu do API lub ochrony pewnych danych na serwerze. W języku Javascript, możemy wykorzystać metodę "fetch" do wysłania żądania z podstawową autoryzacją HTTP. Poniżej znajduje się przykładowy kod, który demonstruje sposób wykorzystania tej metody:

```Javascript
fetch('https://example.com/api/data', {
  method: 'GET',
  headers: {
    'Authorization': 'Basic ' + btoa('username:password'),
    'Content-Type': 'application/json'
  }
}).then(response => response.json())
  .then(data => console.log(data))
  .catch(error => console.error(error));
```
W powyższym przykładzie, "username" i "password" są zastąpione odpowiednimi danymi uwierzytelniającymi. Metoda "btoa()" jest wykorzystywana do zakodowania tych danych w postaci Base64, zgodnej ze specyfikacją autoryzacji HTTP. Możemy również określić rodzaj żądania, tutaj jest to metoda "GET", oraz przesłać ewentualne inne nagłówki, jak w przypadku nagłówka "Content-Type". W dalszej części skryptu możemy zdefiniować, co ma się stać z odpowiedzią i ewentualnymi błędami.

## Głębsze zanurzenie

Podstawowa autoryzacja HTTP działa w prosty sposób - przesyłamy dane uwierzytelniające w nagłówku żądania, a serwer je sprawdza i w zależności od wyniku odpowiedzi, udziela dostępu do danych lub odrzuca żądanie. Bardziej szczegółowe informacje na temat autoryzacji HTTP i sposobów jej implementacji możesz znaleźć w odpowiednich dokumentacjach.

## Zobacz również

- [Dokumentacja podstawowej autoryzacji HTTP](https://developer.mozilla.org/en-US/docs/Web/HTTP/Authentication)
- [Informacje na temat autoryzacji HTTP w Node.js](https://nodejs.org/api/http.html#http_http_authenticate_header)