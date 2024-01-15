---
title:                "Pobieranie strony internetowej"
html_title:           "Javascript: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Dlaczego

Pobieranie stron internetowych jest jednym z podstawowych zadań programistów, którzy pracują z językiem Javascript. Jest to niezbędne do pozyskania danych z sieci, tworzenia automatycznych skryptów lub tworzenia spersonalizowanych aplikacji internetowych. Dzięki temu procesowi możliwe jest uzyskanie wielu informacji, które można później wykorzystać w różnych celach.

## Jak to zrobić

Aby pobrać stronę internetową za pomocą Javascript, można skorzystać z wbudowanej funkcji "fetch". Poniżej przedstawiamy przykładowy kod, który pobierze stronę główną Google i wyświetli jej treść w konsoli.

```Javascript
fetch('https://www.google.com')
.then(response => response.text())
.then(data => console.log(data));
```

Output:

```Text
<!DOCTYPE html>
<html>
<head>
...
</head>
<body>
...
</body>
</html>
```

Możliwe jest również wykorzystanie biblioteki zewnętrznej, takiej jak "axios" lub "request", aby ułatwić proces pobierania. Ważne jest również zrozumienie sposobu budowy zapytań HTTP i przekazywania parametrów, takich jak nagłówki czy dane formularza.

## Pogłębiona analiza

Pobieranie stron internetowych za pomocą Javascript może być skomplikowanym procesem. Wymaga to wiedzy na temat protokołu HTTP, struktury HTML oraz sposobu przetwarzania danych przez przeglądarkę. Dlatego, warto poświęcić czas na dokładne zapoznanie się z dokumentacją i wykorzystanie narzędzi programistycznych, takich jak narzędzia deweloperskie w przeglądarce czy debugger.

## Zobacz również

- [Dokumentacja funkcji "fetch" na MDN](https://developer.mozilla.org/pl/docs/Web/API/Fetch_API/Using_Fetch)
- [Dokumentacja biblioteki "axios"](https://github.com/axios/axios)
- [Dokumentacja biblioteki "request"](https://github.com/request/request)