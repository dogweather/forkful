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

## Co i dlaczego?

Pobieranie strony internetowej to proces pobierania kodu źródłowego danej strony przez przeglądarkę internetową. Programiści często pobierają strony internetowe, aby wyświetlić je w swoim oprogramowaniu lub przetworzyć dane ze strony.

## Jak to zrobić:

```Javascript
fetch('https://example.com')
    .then(response => response.text())
    .then(data => console.log(data));
```

Output:

```
<html>
<head>
    <title>Example</title>
</head>
<body>
    <h1>Hello World!</h1>
</body>
</html>
```

## Głębsze spojrzenie:

Pobieranie stron internetowych jest możliwe dzięki protokołowi HTTP. Przez lata pojawiło się wiele innych metod pobierania danych z internetu, takich jak WebSockets czy API REST, ale pobieranie strony internetowej jest w dalszym ciągu jednym z najbardziej popularnych sposobów. W Javascripcie, funkcja `fetch()` jest wykorzystywana do wysyłania zapytania HTTP do podanego adresu URL i zwraca obietnicę, która potem może być przetworzona przez kolejne funkcje.

## Zobacz też:

- [fetch() na MDN](https://developer.mozilla.org/pl/docs/Web/API/GlobalFetch/fetch)
- [HTTP na Wikipedia](https://pl.wikipedia.org/wiki/HTTP)