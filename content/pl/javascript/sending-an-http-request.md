---
title:                "Wysyłanie zapytania http"
html_title:           "Javascript: Wysyłanie zapytania http"
simple_title:         "Wysyłanie zapytania http"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Dlaczego

Ktoś mógłby zapytać, dlaczego powinniśmy się zainteresować wysyłaniem żądań HTTP w naszym kodzie JavaScript? Otóż, jest to niezbędne do komunikacji z innymi serwerami lub zewnętrznymi źródłami danych, co jest niezbędne w dzisiejszym świecie aplikacji internetowych.

## Jak to zrobić

Aby wysłać żądanie HTTP w kodzie JavaScript, musimy użyć obiektu XMLHttpRequest lub metody fetch. Poniżej znajdują się przykładowe kody, które pokazują, jak zastosować te dwie metody.

##### Przykład z użyciem obiektu XMLHttpRequest:

```Javascript
var xhr = new XMLHttpRequest();
xhr.open("GET", "https://serwer.pl/dane", true);
xhr.send();

xhr.onreadystatechange = function () {
    if (xhr.readyState == 4 && xhr.status == 200) {
        console.log(xhr.responseText);
    }
}
```

##### Przykład z użyciem metody fetch:

```Javascript
fetch("https://serwer.pl/dane")
    .then(response => response.json())
    .then(data => console.log(data));
```

W wyniku tych kodów otrzymamy odpowiedź z serwera w postaci tekstu lub danych w formacie JSON.

## Zagłębienie się w temat

Wysyłanie żądania HTTP za pomocą JavaScript może być skomplikowane ze względu na różnice w obsłudze przez różne przeglądarki. Dlatego, warto rozważyć użycie bibliotek takich jak Axios lub jQuery, które ułatwiają pracę z żądaniami.

Każde żądanie HTTP składa się z różnych części, takich jak metoda (GET, POST, PUT, DELETE), adres URL i opcjonalnie ciało żądania. Możemy również dodać nagłówki do żądania, które zawierają dodatkowe informacje dla serwera.

W przypadku żądań typu POST lub PUT, musimy przekazać dane do serwera w formacie JSON lub FormData. W przypadku używania metody fetch, możemy określić ustawienia zapytania, takie jak metoda, nagłówki i dane.

Teraz, gdy znasz podstawy wysyłania żądań HTTP w JavaScript, możesz z powodzeniem komunikować się z innymi serwerami i pobierać dane z zewnętrznych źródeł.

## Zobacz również

- [Using XMLHttpRequest](https://developer.mozilla.org/en-US/docs/Web/API/XMLHttpRequest/Using_XMLHttpRequest)
- [Using Fetch](https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API/Using_Fetch)