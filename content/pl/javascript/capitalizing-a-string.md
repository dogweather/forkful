---
title:                "Konwertowanie ciągu tekstowego na wielkie litery"
html_title:           "Javascript: Konwertowanie ciągu tekstowego na wielkie litery"
simple_title:         "Konwertowanie ciągu tekstowego na wielkie litery"
programming_language: "Javascript"
category:             "Javascript"
tag:                  "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/capitalizing-a-string.md"
---

{{< edit_this_page >}}

Co to jest kapitalizacja łańcucha i dlaczego powinniśmy tego używać?

Kapitalizacja łańcucha jest procesem zmiany małych liter na wielkie w łańcuchu tekstu. Programiści często wykonują tę operację, aby poprawić wygląd lub czytelność tekstu, a także dla potrzeb sortowania danych.

Jak to zrobić?

```Javascript
let przykladowyTekst = "to jest przykladowy tekst";

//Funkcja toUpperCase () zwraca nowy łańcuch z wielkimi literami
console.log(przykladowyTekst.toUpperCase ()); // WYNIK: TO JEST PRZYKŁADOWY TEKST

//Możesz również użyć metody charAt (), aby zmienić pierwszą literę na wielką
console.log(przykladowyTekst.charAt(0).toUpperCase() + przykladowyTekst.slice(1)); //WYNIK: To jest przykladowy tekst
```

Także warto wiedzieć

W historii programowania istniało wiele innych sposobów zmiany wielkości liter w łańcuchu, m.in. metody takie jak "snake_case" lub "camelCase", ale kapitalizacja jest najbardziej powszechna.

Alternatywą dla kapitalizacji jest operacja "toLowerCase ()", która zamienia wszystkie litery na małe. 

W przypadku implementacji, metoda toUpperCase () jest niezwykle szybka i wydajna, ale możesz również użyć biblioteki zewnętrznej, jeśli potrzebujesz bardziej zaawansowanych funkcji kapitalizacji.

Zobacz również

- Dokumentacja JavaScript na temat metod string (https://developer.mozilla.org/pl/docs/Web/JavaScript/Referencje/Obiekty/String)
- Inne sposoby manipulacji danymi w JavaScript (https://www.w3schools.com/js/js_string_methods.asp)
- Przydatne przykłady kapitalizacji w różnych językach programowania (https://www.programiz.com/java-programming/examples/string-capitalization)