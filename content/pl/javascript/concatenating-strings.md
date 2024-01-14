---
title:                "Javascript: Łączenie łańcuchów znaków"
programming_language: "Javascript"
category:             "Strings"
editURL:              "https://github.com/dogweather/forkful/blob/master/content/pl/javascript/concatenating-strings.md"
---

{{< edit_this_page >}}

# Dlaczego skręcanie ciągów znaków w JavaScript jest ważne?

Skręcanie, czyli łączenie, ciągów znaków jest ważnym elementem w programowaniu w JavaScript. Pozwala na łączenie różnych zmiennych i tekstu w jeden ciąg, co jest niezbędne w wielu przypadkach. Na przykład, jeśli chcemy wyświetlić imię i nazwisko użytkownika w jednym komunikacie, to musimy skleić dwa ciągi znaków - zmienną z imieniem i zmienną z nazwiskiem.

# Jak to zrobić?

Skręcanie ciągów znaków w JavaScript jest bardzo proste. Możemy tego dokonać poprzez użycie operatora "+" lub metody `.concat()`. Oba te sposoby działają w taki sam sposób - łączą podane ciągi znaków w jeden. Przykładowe użycie:

```Javascript
let imie = "Jan";
let nazwisko = "Kowalski";
let pelneImie = imie + " " + nazwisko; // pelenImie = "Jan Kowalski"
let innaforma = imie.concat(" ", nazwisko); // innaforma = "Jan Kowalski"
```

Pamiętajmy, że jeśli nie użyjemy spacji w skłądania ciągów, to otrzymamy po prostu połączenie dwóch słów bez przerwy, co w niektórych przypadkach może być niepożądane.

# Głębsza analiza

W przypadkach, gdy potrzebujemy połączyć więcej niż dwa ciągi znaków, możemy stosować różne podejścia. Jednym z nich jest użycie operatora "+" wielokrotnie, np.:

```Javascript
let pierwszy = "Ala";
let srodkowy = "ma";
let trzeci = "kota";
let caly = pierwszy + " " + srodkowy + " " + trzeci; // caly = "Ala ma kota"
```

Innym sposobem jest użycie metody `.join()`, która łączy tablicę ciągów znaków w jeden, używając jako separatora podanego argumentu. Przykład:

```Javascript
let zdanie = ["Algorytm", "jest", "bardzo", "ważny"];
let sklejone = zdanie.join(" "); // sklejone = "Algorytm jest bardzo ważny"
```

Warto również pamiętać, że skręcanie ciągów znaków ma wpływ na wydajność kodu. Używanie operatora "+" wielokrotnie może powodować wolniejsze działanie programu. W takich przypadkach lepiej jest zastosować metodę `.join()` lub postawić na inne podejście do problemu. 

# Zobacz także

- [MDN web docs - String concatenation](https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Operators/Addition_assignment)
- [W3Schools - JavaScript String concatenation](https://www.w3schools.com/js/js_string_concat.asp)
- [The Modern JavaScript Tutorial - String concatenation](https://javascript.info/string)
- [Codecademy - String Concatenation](https://www.codecademy.com/learn/learn-javascript/modules/learn-javascript-strings/cheatsheet)