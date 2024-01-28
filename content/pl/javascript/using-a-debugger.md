---
title:                "Korzystanie z debugera"
date:                  2024-01-26T03:50:22.616368-07:00
model:                 gpt-4-0125-preview
simple_title:         "Korzystanie z debugera"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Testing and Debugging"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/using-a-debugger.md"
---

{{< edit_this_page >}}

## Co i dlaczego?
Korzystanie z debugera oznacza sięgnięcie po specjalistyczne narzędzia, które pozwalają zajrzeć pod maskę kodu, obserwując jego działanie krok po kroku. Programiści robią to, aby wyłapywać błędy, optymalizować wydajność i rozumieć zachowanie kodu.

## Jak to zrobić:
Oto fragment kodu JavaScript, który nie działa zgodnie z oczekiwaniami:

```javascript
function buggyMultiply(a, b) {
    return a + b; // Ups! To powinna być mnożenie, a nie dodawanie.
}

let result = buggyMultiply(5, 3);
console.log('Wynik:', result);
```

Wynik jest niepoprawny:
```
Wynik: 8
```

Debugowanie w narzędziach deweloperskich Chrome:

1. Otwórz ten JS w przeglądarce.
2. Kliknij prawym przyciskiem i wybierz "Zbadaj" aby otworzyć DevTools.
3. Kliknij zakładkę "Źródła".
4. Znajdź swój fragment kodu lub stronę i ustaw punkt przerwania, klikając numer linii obok instrukcji `return`.
5. Odśwież stronę, aby uruchomić punkt przerwania.
6. Sprawdź panel "Zakres" aby zobaczyć lokalne zmienne `a` i `b`.
7. Przesuń się za pomocą przycisku "Przejdź do następnej funkcji".
8. Znajdź błąd w instrukcji `return`.
9. Popraw kod:
```javascript
function buggyMultiply(a, b) {
    return a * b; // Poprawione!
}

let result = buggyMultiply(5, 3);
console.log('Wynik:', result);
```

Poprawiony wynik:
```
Wynik: 15
```

## Pogłębiona analiza
Koncepcja debugowania istnieje od wczesnych dni komputeryzacji—legenda mówi, że zaczęła się, kiedy w komputerze znaleziono ćmę w latach 40-tych! Dzisiaj, debugery JavaScript, takie jak wbudowane narzędzia przeglądarek (Chrome DevTools, Narzędzia deweloperskie Firefox) czy debugery zintegrowane z IDE (Visual Studio Code, WebStorm) oferują mnóstwo funkcji.

Alternatywami dla wbudowanych debuggerów są narzędzia stron trzecich takie jak WebStorm lub korzystanie ze starego dobrego `console.log` do wypisywania stanów zmiennych. Jednak nie oferują one interakcji w czasie rzeczywistym i szczegółowej inspekcji dostępnej w debuggerach.

Jeśli chodzi o szczegóły implementacji, większość debuggerów działa podobnie: pozwalają na ustawianie punktów przerwania, które zatrzymują wykonanie, krokowe przeglądanie kodu, inspekcję bieżących stanów zmiennych, obserwowanie wyrażeń, a nawet manipulację wartościami na bieżąco, aby testować różne scenariusze.

## Zobacz również
- [Google Chrome DevTools](https://developers.google.com/web/tools/chrome-devtools)
- [Mozilla Developer Network - Debugger Firefox](https://developer.mozilla.org/en-US/docs/Tools/Debugger)
- [Visual Studio Code - Debugowanie](https://code.visualstudio.com/docs/editor/debugging)
