---
title:                "Wysyłanie żądania http"
html_title:           "Arduino: Wysyłanie żądania http"
simple_title:         "Wysyłanie żądania http"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/sending-an-http-request.md"
---

{{< edit_this_page >}}

## Co i dlaczego?

Wysyłanie żądania HTTP to proces komunikacji z serwisem lub aplikacją internetową, które zwraca potrzebne dane. Programiści wykonują to, aby pobierać, przesyłać, aktualizować i usuwać informacje z serwerów.

## Jak to zrobić?

W JavaScript można wysyłać żądania HTTP na różne sposoby, ale jednym z najpopularniejszych jest używanie funkcji `fetch()`. Zobaczmy przykład:

```Javascript
fetch('https://api.example.com/data', {
    method: 'GET', 
})
.then(response => response.json())
.then(data => console.log(data))
.catch((error) => console.log('Error:', error));
```

Tutaj najpierw wywołujemy funkcję `fetch()` z URL endpointu, który chcemy wywołać, a następnie definiujemy metodę żądania jako 'GET'. Po otrzymaniu odpowiedzi, konwertujemy ją na JSON, a następnie wyświetlamy dane.

## Deep Dive

Historycznie, do wysyłania żądań HTTP w JavaScript używano obiektu XMLHttpRequest. Ale obecnie `fetch()` jest preferowaną metodą ze względu na jej prostotę i łatwość użycia.

Alternatywą dla `fetch()` jest biblioteka o nazwie `axios`, która również oferuje prosty sposób na wykonywanie żądań HTTP.

Jednym z ważnych aspektów wysyłania żądań HTTP jest obsługa błędów. W powyższym przykładzie użyliśmy `catch()`, aby obsługiwać wszelkie potencjalne błędy, które mogą wystąpić podczas wykonywania żądania.

## Zobacz także

1. [Fetch API na MDN Web Docs](https://developer.mozilla.org/pl/docs/Web/API/Fetch_API/Using_Fetch)
2. [Axios - Biblioteka do żądań HTTP](https://axios-http.com/docs/intro)
3. [Obsługa błędów w JavaScript na MDN](https://developer.mozilla.org/pl/docs/Web/JavaScript/Guide/Control_flow_and_error_handling)