---
title:                "Pobieranie strony internetowej"
html_title:           "TypeScript: Pobieranie strony internetowej"
simple_title:         "Pobieranie strony internetowej"
programming_language: "TypeScript"
category:             "TypeScript"
tag:                  "HTML and the Web"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/typescript/downloading-a-web-page.md"
---

{{< edit_this_page >}}

## Dlaczego

Pobieranie strony internetowej może być przydatne, jeśli potrzebujesz łatwego dostępu do jej zawartości lub chcesz przeprowadzić analizę danych. Może to również służyć jako podstawa do tworzenia własnych narzędzi lub aplikacji internetowych.

## Jak to zrobić

Pobieranie strony internetowej w języku TypeScript jest bardzo prosty i wymaga tylko kilku linijek kodu. Wystarczy użyć wbudowanego modułu `http` oraz funkcji `get()`. Poniżej znajduje się przykładowy kod, który pobiera zawartość strony internetowej i wyświetla ją w konsoli:

```TypeScript
import * as http from 'http';

http.get('https://www.example.com', res => {
    res.setEncoding('utf8');
    let data = '';

    res.on('data', chunk => {
        data += chunk;
    });

    res.on('end', () => {
        console.log(data);
    });
});
```

W tym przykładzie, najpierw importujemy moduł `http`. Następnie wywołujemy funkcję `get()` z parametrem zawierającym adres URL strony, którą chcemy pobrać. Funkcja ta przyjmuje również funkcję zwrotną, która zostanie uruchomiona, gdy strona zostanie pobrana. W tej funkcji ustawiamy typ kodowania na `utf8` i deklarujemy zmienną `data`, która będzie przechowywać pobraną zawartość strony. W kolejnych dwóch funkcjach `on()` podłączamy funkcje obsługujące zdarzenia pobierania danych i zakończenia pobierania. W funkcji obsługującej zdarzenie pobierania danych, do zmiennej `data` dodajemy kolejne porcje danych. A w funkcji obsługującej zdarzenie zakończenia, wyświetlamy całą zawartość strony w konsoli.

## Deep Dive

Pobieranie strony internetowej w języku TypeScript daje nam wiele możliwości. Możemy manipulować pobranymi danymi, przetwarzać je i wykorzystywać do różnych celów. Na przykład, możemy przekazać pobraną zawartość do funkcji `writeFile()` z wbudowanego modułu `fs`, aby zapisać ją w pliku na naszym komputerze. Możemy również analizować pobrane dane w poszukiwaniu konkretnych informacji lub wykorzystać je do automatyzacji zadań internetowych.

## Zobacz także

- Dokumentacja modułu `http` w języku TypeScript: https://nodejs.org/dist/latest-v14.x/docs/api/http.html
- Przykładowe projekty wykorzystujące pobieranie strony internetowej w języku TypeScript: https://github.com/microsoft/TypeScript-Node-Starter/issues/45https://github.com/aszaharia/Node-TS-Samples/tree/master/src
- Przykładowe działające kodu pobierającego stronę internetową w języku TypeScript: https://github.com/how-to-node/exercise/tree/master/request-simple