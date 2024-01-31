---
title:                "Pobieranie strony internetowej"
date:                  2024-01-20T17:44:27.587174-07:00
model:                 gpt-4-1106-preview
simple_title:         "Pobieranie strony internetowej"

tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## What & Why? (Co i Dlaczego?)
Pobieranie strony internetowej to proces ściągania jej treści, by móc z nią interaktywnie pracować poza przeglądarką. Programiści robią to, aby analizować dane, testować wydajność lub archiwizować zawartość.

## How to: (Jak to zrobić:)
JavaScript pozwala na pobieranie stron internetowych za pomocą Fetch API, które jest obecnie standardem. Poniżej znajduje się prosty przykład użycia:

```javascript
// Zainicjowanie pobierania strony
fetch('https://example.com')
  .then(response => {
    // Sprawdź status odpowiedzi
    if (response.ok) return response.text();
    throw new Error('Pobieranie nie powiodło się: ' + response.status);
  })
  .then(html => {
    // Tutaj masz pobraną treść jako HTML
    console.log(html);
  })
  .catch(error => {
    // Obsługa błędów
    console.error('Wystąpił błąd', error);
  });
```

Przykładowy output:

```
<!doctype html>
<html>
<head>
    <title>Przykładowa strona</title>
...
</head>
<body>
    <p>Jest to przykładowa zawartość strony</p>
...
</body>
</html>
```

## Deep Dive (Dogłębna analiza)
Fetch API, które zastąpiło XMLHttpRequest, stanowi obecnie nową erę asynchronicznych żądań w JavaScript. Historycznie, pobieranie stron polegało na użyciu technologii takich jak iframe lub AJAX. Alternatywą dla Fetch może być Axios, biblioteka oparta na promisach, która oferuje nieco szersze możliwości konfiguracji.

Implementację warto rozszerzać o obsługę błędów, timeoutów i limitów czasu odpowiedzi. Fetch API samo z siebie nie obsługuje timeoutów, ale można dokonać tej funkcjonalności za pomocą dodatkowego kodu. Jest to szczególnie ważne podczas pracy z dużymi obciążeniami lub niestabilnymi połączeniami internetowymi.

## See Also (Zobacz także)
- MDN Web Docs na temat Fetch API: https://developer.mozilla.org/en-US/docs/Web/API/Fetch_API
- Porównanie Fetch i Axios: https://www.npmjs.com/package/axios
- Praktyczne porady dotyczące Fetch API: https://davidwalsh.name/fetch

Wskazane źródła pozwolą na dalsze zgłębianie tematu i rozszerzenie wiedzy o szczegółowe aspekty pobierania treści stron oraz bardziej zaawansowane przypadki użycia.
